use crate::backend::JitSession;
use crate::ir::bytecode::BytecodeChunk;
use crate::runtime::Value;

const JIT_HOT_CALL_THRESHOLD: u32 = 32;

pub struct JitTiering {
    session: Option<JitSession>,
    call_hot_counts: Vec<u32>,
}

impl JitTiering {
    pub fn new(program_chunk_count: usize, enabled: bool) -> Self {
        Self {
            session: if enabled {
                Some(JitSession::new())
            } else {
                None
            },
            call_hot_counts: vec![0; program_chunk_count],
        }
    }

    pub fn maybe_execute(&mut self, func_idx: usize, chunk: &BytecodeChunk) -> Option<Value> {
        if let Some(jit) = self.session.as_ref()
            && let Some(result) = jit.try_invoke_compiled(func_idx)
        {
            return Some(Self::jit_i64_to_value(result));
        }

        let hot_count = self.call_hot_counts.get_mut(func_idx)?;
        *hot_count = hot_count.saturating_add(1);
        if *hot_count < JIT_HOT_CALL_THRESHOLD {
            return None;
        }

        let jit = self.session.as_mut()?;
        match jit.try_compile(func_idx, chunk) {
            Ok(Some(result)) => Some(Self::jit_i64_to_value(result)),
            Ok(None) | Err(_) => None,
        }
    }

    #[inline(always)]
    fn jit_i64_to_value(result: i64) -> Value {
        Value::Int(result.clamp(i32::MIN as i64, i32::MAX as i64) as i32)
    }
}
