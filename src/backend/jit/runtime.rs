use lamina::ir::{IRBuilder, PrimitiveType, Type, builder::i64};
use lamina::mir::codegen::from_ir;
use lamina::runtime::{RuntimeResult, compile_to_runtime};
use lamina::target::Target;

use super::error::BackendError;

pub fn build_constant_main_module(value: i64) -> lamina::ir::Module<'static> {
    let mut builder = IRBuilder::new();
    builder
        .function("main", Type::Primitive(PrimitiveType::I64))
        .export()
        .ret(Type::Primitive(PrimitiveType::I64), i64(value));
    builder.build()
}

fn compile_module_to_runtime_result(
    module: &lamina::ir::Module<'static>,
) -> Result<RuntimeResult, BackendError> {
    let mir_module = from_ir(module, "jit").map_err(|e| BackendError::Lamina(e.into()))?;
    let target = Target::detect_host();
    compile_to_runtime(
        &mir_module,
        target.architecture,
        target.operating_system,
        Some("main"),
    )
    .map_err(BackendError::Lamina)
}

pub struct CompiledChunk {
    runtime: RuntimeResult,
    entry: unsafe extern "C" fn() -> i64,
}

impl CompiledChunk {
    pub fn from_module(module: &lamina::ir::Module<'static>) -> Result<Self, BackendError> {
        let runtime = compile_module_to_runtime_result(module)?;
        if runtime.function_ptr.is_null() {
            return Err(BackendError::Process(
                "runtime JIT returned a null function pointer".to_string(),
            ));
        }
        let entry = unsafe {
            std::mem::transmute::<*const u8, unsafe extern "C" fn() -> i64>(runtime.function_ptr)
        };
        Ok(Self { runtime, entry })
    }

    pub fn invoke(&self) -> i64 {
        let _keep_alive = &self.runtime;
        unsafe { (self.entry)() }
    }
}
