use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use lamina::LaminaError;
use lamina::ir::{IRBuilder, PrimitiveType, Type, builder::i64};
use lamina::mir::codegen::from_ir;
use lamina::mir_codegen::generate_mir_to_target;
use lamina::target::Target;

use crate::frontend::Lexer;
use crate::frontend::TokenType;
use crate::ir::bytecode::{BytecodeChunk, ConstEntry};

#[derive(Debug)]
pub enum BackendError {
    Io(std::io::Error),
    Lamina(LaminaError),
    Parse(String),
    Process(String),
}

impl std::fmt::Display for BackendError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BackendError::Io(e) => write!(f, "I/O error: {}", e),
            BackendError::Lamina(e) => write!(f, "Lamina error: {}", e),
            BackendError::Parse(msg) => write!(f, "Parse error: {}", msg),
            BackendError::Process(msg) => write!(f, "Process error: {}", msg),
        }
    }
}

impl std::error::Error for BackendError {}

impl From<std::io::Error> for BackendError {
    fn from(err: std::io::Error) -> Self {
        BackendError::Io(err)
    }
}

impl From<LaminaError> for BackendError {
    fn from(err: LaminaError) -> Self {
        BackendError::Lamina(err)
    }
}

pub fn extract_constant_return(js_source: &str) -> Result<i64, BackendError> {
    let mut lexer = Lexer::new(js_source.to_string());
    let tokens = lexer.tokenize();

    let mut in_main_function = false;
    let mut return_value: Option<i64> = None;

    for (i, token) in tokens.iter().enumerate() {
        match &token.token_type {
            TokenType::Function => {
                if i + 1 < tokens.len()
                    && matches!(tokens[i + 1].token_type, TokenType::Identifier)
                    && tokens[i + 1].lexeme == "main"
                {
                    in_main_function = true;
                }
            }
            TokenType::Return => {
                if in_main_function {
                    for token in tokens.iter().skip(i + 1) {
                        if matches!(token.token_type, TokenType::Number)
                            && let Ok(val) = token.lexeme.parse::<i64>()
                        {
                            return_value = Some(val);
                            break;
                        }
                    }
                }
            }
            TokenType::RightBrace => {
                in_main_function = false;
            }
            _ => {}
        }
    }

    return_value.ok_or_else(|| {
        BackendError::Parse("Could not find main function with numeric return".to_string())
    })
}

fn build_constant_main_module(value: i64) -> lamina::ir::Module<'static> {
    let mut builder = IRBuilder::new();
    builder
        .function("main", Type::Primitive(PrimitiveType::I64))
        .export()
        .ret(Type::Primitive(PrimitiveType::I64), i64(value));
    builder.build()
}

fn compile_module_to_assembly<W: std::io::Write>(
    module: &lamina::ir::Module<'static>,
    output: &mut W,
) -> Result<(), BackendError> {
    let mir_module = from_ir(module, "jit").map_err(|e| BackendError::Lamina(e.into()))?;
    let target = Target::detect_host();
    generate_mir_to_target(
        &mir_module,
        output,
        target.architecture,
        target.operating_system,
    )
    .map_err(BackendError::Lamina)?;
    Ok(())
}

pub fn translate_to_lamina_ir(js_source: &str) -> Result<String, BackendError> {
    let value = extract_constant_return(js_source)?;
    let ir = format!(
        concat!(
            "@export\n",
            "fn @main() -> i64 {{\n",
            "  entry:\n",
            "    ret.i64 {}\n",
            "}}\n"
        ),
        value
    );
    Ok(ir)
}

fn assemble_object(asm_path: &Path, obj_path: &Path) -> Result<(), BackendError> {
    let compiler = if cfg!(target_os = "macos") {
        "clang"
    } else {
        "gcc"
    };
    let status = Command::new(compiler)
        .arg("-c")
        .arg("-fPIC")
        .arg(asm_path)
        .arg("-o")
        .arg(obj_path)
        .status()?;
    if !status.success() {
        return Err(BackendError::Process(
            "failed to assemble object".to_string(),
        ));
    }
    Ok(())
}

fn link_shared(obj_path: &Path, so_path: &Path) -> Result<(), BackendError> {
    let compiler = if cfg!(target_os = "macos") {
        "clang"
    } else {
        "gcc"
    };
    let mut cmd = Command::new(compiler);
    if cfg!(target_os = "macos") {
        cmd.arg("-dynamiclib");
    } else {
        cmd.arg("-shared");
    }
    let status = cmd.arg(obj_path).arg("-o").arg(so_path).status()?;
    if !status.success() {
        return Err(BackendError::Process(
            "failed to link shared library".to_string(),
        ));
    }
    Ok(())
}

#[cfg(target_os = "linux")]
#[link(name = "dl")]
#[allow(dead_code)]
unsafe extern "C" {
    fn dlopen(filename: *const core::ffi::c_char, flag: i32) -> *mut core::ffi::c_void;
    fn dlsym(
        handle: *mut core::ffi::c_void,
        symbol: *const core::ffi::c_char,
    ) -> *mut core::ffi::c_void;
    fn dlclose(handle: *mut core::ffi::c_void) -> i32;
    fn dlerror() -> *const core::ffi::c_char;
}

#[cfg(target_os = "macos")]
#[allow(dead_code)]
unsafe extern "C" {
    fn dlopen(filename: *const core::ffi::c_char, flag: i32) -> *mut core::ffi::c_void;
    fn dlsym(
        handle: *mut core::ffi::c_void,
        symbol: *const core::ffi::c_char,
    ) -> *mut core::ffi::c_void;
    fn dlclose(handle: *mut core::ffi::c_void) -> i32;
    fn dlerror() -> *const core::ffi::c_char;
}

const RTLD_NOW: i32 = 0x2;

fn dl_last_error() -> Option<String> {
    unsafe {
        let err = dlerror();
        if err.is_null() {
            None
        } else {
            Some(CStr::from_ptr(err).to_string_lossy().into_owned())
        }
    }
}

fn jit_invoke(so_path: &Path, symbol: &str) -> Result<i64, BackendError> {
    let so_str = so_path
        .to_str()
        .ok_or_else(|| BackendError::Parse("library path is not valid UTF-8".to_string()))?;
    let so_c = CString::new(so_str)
        .map_err(|_| BackendError::Parse("invalid library path".to_string()))?;
    let sym_c =
        CString::new(symbol).map_err(|_| BackendError::Parse("invalid symbol".to_string()))?;

    let handle = unsafe { dlopen(so_c.as_ptr(), RTLD_NOW) };
    if handle.is_null() {
        let msg = dl_last_error().unwrap_or_else(|| "dlopen failed".to_string());
        return Err(BackendError::Process(msg));
    }

    let _ = dl_last_error();
    let sym = unsafe { dlsym(handle, sym_c.as_ptr()) };
    if let Some(err) = dl_last_error() {
        unsafe { dlclose(handle) };
        return Err(BackendError::Process(err));
    }

    let func: extern "C" fn() -> i64 = unsafe { std::mem::transmute(sym) };
    let result = func();
    unsafe { dlclose(handle) };
    Ok(result)
}

fn jit_invoke_any(so_path: &Path, symbols: &[&str]) -> Result<i64, BackendError> {
    let mut last: Option<BackendError> = None;
    for s in symbols {
        match jit_invoke(so_path, s) {
            Ok(v) => return Ok(v),
            Err(e) => last = Some(e),
        }
    }
    Err(last
        .unwrap_or_else(|| BackendError::Process("dlsym failed for all candidates".to_string())))
}

pub fn run_via_jit(js_source: &str) -> Result<i64, BackendError> {
    let mut tmp_dir: PathBuf = std::env::temp_dir();
    tmp_dir.push(format!("jsina_jit_{}", std::process::id()));
    if !tmp_dir.exists() {
        fs::create_dir_all(&tmp_dir)?;
    }

    let value = extract_constant_return(js_source)?;
    let module = build_constant_main_module(value);
    let so_path = compile_and_link(&module, &tmp_dir, "jit")?;
    let result = jit_invoke_any(&so_path, &["_main", "main"])?;

    let _ = fs::remove_file(&tmp_dir.join("jit.s"));
    let _ = fs::remove_file(&tmp_dir.join("jit.o"));
    let _ = fs::remove_file(&so_path);

    Ok(result)
}

fn compile_and_link(
    module: &lamina::ir::Module<'static>,
    tmp_dir: &Path,
    base_name: &str,
) -> Result<PathBuf, BackendError> {
    let asm_path = tmp_dir.join(format!("{}.s", base_name));
    let obj_path = tmp_dir.join(format!("{}.o", base_name));
    let so_name = if cfg!(target_os = "macos") {
        format!("lib{}.dylib", base_name)
    } else {
        format!("lib{}.so", base_name)
    };
    let so_path = tmp_dir.join(&so_name);

    let mut asm_buf: Vec<u8> = Vec::new();
    compile_module_to_assembly(module, &mut asm_buf)?;
    fs::write(&asm_path, &asm_buf)?;

    assemble_object(&asm_path, &obj_path)?;
    link_shared(&obj_path, &so_path)?;

    Ok(so_path)
}

pub struct JitSession {
    tmp_dir: PathBuf,
    cache: HashMap<usize, PathBuf>,
}

impl JitSession {
    pub fn new() -> Self {
        let mut tmp_dir: PathBuf = std::env::temp_dir();
        tmp_dir.push(format!("jsina_jit_{}", std::process::id()));
        if !tmp_dir.exists() {
            let _ = fs::create_dir_all(&tmp_dir);
        }
        Self {
            tmp_dir,
            cache: HashMap::new(),
        }
    }

    pub fn try_compile(
        &mut self,
        chunk_index: usize,
        chunk: &BytecodeChunk,
    ) -> Result<Option<i64>, BackendError> {
        if self.cache.contains_key(&chunk_index) {
            let so_path = self.cache.get(&chunk_index).expect("cached");
            return Ok(Some(jit_invoke_any(so_path, &["_main", "main"])?));
        }

        let Some(module) = bytecode_to_lamina_trivial(chunk) else {
            return Ok(None);
        };

        let base_name = format!("chunk_{}", chunk_index);
        let so_path = compile_and_link(&module, &self.tmp_dir, &base_name)?;
        self.cache.insert(chunk_index, so_path.clone());

        let result = jit_invoke_any(&so_path, &["_main", "main"])?;
        Ok(Some(result))
    }

    pub fn has_compiled(&self, chunk_index: usize) -> bool {
        self.cache.contains_key(&chunk_index)
    }

    pub fn invoke_compiled(&self, chunk_index: usize) -> Result<i64, BackendError> {
        let so_path = self
            .cache
            .get(&chunk_index)
            .ok_or_else(|| BackendError::Process("chunk not compiled".to_string()))?;
        jit_invoke_any(so_path, &["_main", "main"])
    }
}

fn bytecode_to_lamina_trivial(chunk: &BytecodeChunk) -> Option<lamina::ir::Module<'static>> {
    let code = &chunk.code;
    let constants = &chunk.constants;

    if chunk.num_locals != 0 || !chunk.handlers.is_empty() || chunk.rest_param_index.is_some() {
        return None;
    }

    let mut pc = 0;
    let mut stack: Vec<i64> = Vec::new();

    while pc < code.len() {
        let op = code[pc];
        pc += 1;

        match op {
            0x01 => {
                let idx = code.get(pc).copied().unwrap_or(0) as usize;
                pc += 1;
                let val = match constants.get(idx)? {
                    ConstEntry::Int(n) => *n,
                    _ => return None,
                };
                stack.push(val);
            }
            0x10 => {
                let b = stack.pop()?;
                let a = stack.pop()?;
                stack.push(a.saturating_add(b));
            }
            0x11 => {
                let b = stack.pop()?;
                let a = stack.pop()?;
                stack.push(a.saturating_sub(b));
            }
            0x12 => {
                let b = stack.pop()?;
                let a = stack.pop()?;
                stack.push(a.saturating_mul(b));
            }
            0x20 => {
                let result = stack.pop().unwrap_or(0);
                if pc != code.len() {
                    return None;
                }
                return Some(build_constant_main_module(result));
            }
            _ => return None,
        }
    }

    let result = stack.pop().unwrap_or(0);
    Some(build_constant_main_module(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::bytecode::ConstEntry;

    #[test]
    fn translate_simple_main() {
        let ir = translate_to_lamina_ir("function main() { return 42; }").expect("translate");
        assert!(ir.contains("ret.i64 42"));
        assert!(ir.contains("@main"));
    }

    #[test]
    fn jit_session_trivial_add() {
        let chunk = BytecodeChunk {
            code: vec![0x01, 0, 0x01, 1, 0x10, 0x20],
            constants: vec![ConstEntry::Int(10), ConstEntry::Int(32)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let mut jit = JitSession::new();
        let result = jit.try_compile(0, &chunk).expect("compile");
        assert_eq!(result, Some(42));
    }
}
