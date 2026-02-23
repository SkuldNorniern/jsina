use std::ffi::{CStr, CString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use lamina::{compile_lamina_ir_to_assembly, error::LaminaError};

use crate::frontend::Lexer;
use crate::frontend::TokenType;

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

pub fn translate_to_lamina_ir(js_source: &str) -> Result<String, BackendError> {
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

    let value = return_value.ok_or_else(|| BackendError::Parse(
        "Could not find main function with numeric return".to_string(),
    ))?;

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
    let compiler = if cfg!(target_os = "macos") { "clang" } else { "gcc" };
    let status = Command::new(compiler)
        .arg("-c")
        .arg("-fPIC")
        .arg(asm_path)
        .arg("-o")
        .arg(obj_path)
        .status()?;
    if !status.success() {
        return Err(BackendError::Process("failed to assemble object".to_string()));
    }
    Ok(())
}

fn link_shared(obj_path: &Path, so_path: &Path) -> Result<(), BackendError> {
    let compiler = if cfg!(target_os = "macos") { "clang" } else { "gcc" };
    let mut cmd = Command::new(compiler);
    if cfg!(target_os = "macos") {
        cmd.arg("-dynamiclib");
    } else {
        cmd.arg("-shared");
    }
    let status = cmd.arg(obj_path).arg("-o").arg(so_path).status()?;
    if !status.success() {
        return Err(BackendError::Process("failed to link shared library".to_string()));
    }
    Ok(())
}

#[cfg(target_os = "linux")]
#[link(name = "dl")]
#[allow(dead_code)]
unsafe extern "C" {
    fn dlopen(filename: *const core::ffi::c_char, flag: i32) -> *mut core::ffi::c_void;
    fn dlsym(handle: *mut core::ffi::c_void, symbol: *const core::ffi::c_char) -> *mut core::ffi::c_void;
    fn dlclose(handle: *mut core::ffi::c_void) -> i32;
    fn dlerror() -> *const core::ffi::c_char;
}

#[cfg(target_os = "macos")]
#[allow(dead_code)]
unsafe extern "C" {
    fn dlopen(filename: *const core::ffi::c_char, flag: i32) -> *mut core::ffi::c_void;
    fn dlsym(handle: *mut core::ffi::c_void, symbol: *const core::ffi::c_char) -> *mut core::ffi::c_void;
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
    let so_c = CString::new(so_str).map_err(|_| BackendError::Parse("invalid library path".to_string()))?;
    let sym_c = CString::new(symbol).map_err(|_| BackendError::Parse("invalid symbol".to_string()))?;

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
    Err(last.unwrap_or_else(|| BackendError::Process("dlsym failed for all candidates".to_string())))
}

pub fn run_via_jit(js_source: &str) -> Result<i64, BackendError> {
    let mut tmp_dir: PathBuf = std::env::temp_dir();
    tmp_dir.push(format!("jsina_jit_{}", std::process::id()));
    if !tmp_dir.exists() {
        fs::create_dir_all(&tmp_dir)?;
    }

    let asm_path = {
        let mut p = tmp_dir.clone();
        p.push("jit.s");
        p
    };
    let obj_path = {
        let mut p = tmp_dir.clone();
        p.push("jit.o");
        p
    };
    let so_path = {
        let mut p = tmp_dir.clone();
        p.push(if cfg!(target_os = "macos") {
            "libjit.dylib"
        } else {
            "libjit.so"
        });
        p
    };

    let lamina_ir = translate_to_lamina_ir(js_source)?;

    let mut asm_buf: Vec<u8> = Vec::new();
    compile_lamina_ir_to_assembly(&lamina_ir, &mut asm_buf)?;
    fs::write(&asm_path, &asm_buf)?;

    assemble_object(&asm_path, &obj_path)?;
    link_shared(&obj_path, &so_path)?;

    let result = jit_invoke_any(&so_path, &["_main", "main"])?;

    let _ = fs::remove_file(&asm_path);
    let _ = fs::remove_file(&obj_path);
    let _ = fs::remove_file(&so_path);

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translate_simple_main() {
        let ir = translate_to_lamina_ir("function main() { return 42; }").expect("translate");
        assert!(ir.contains("ret.i64 42"));
        assert!(ir.contains("@main"));
    }
}
