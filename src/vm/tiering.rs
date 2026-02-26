use crate::backend::JitSession;
use crate::ir::bytecode::BytecodeChunk;
use crate::runtime::Value;

const JIT_HOT_CALL_THRESHOLD: u32 = 32;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ChunkTierState {
    Interpreting,
    Compiled,
    Rejected,
}

pub struct JitTiering {
    session: Option<JitSession>,
    call_hot_counts: Vec<u32>,
    chunk_states: Vec<ChunkTierState>,
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
            chunk_states: vec![ChunkTierState::Interpreting; program_chunk_count],
        }
    }

    #[inline(always)]
    pub fn maybe_execute(&mut self, func_idx: usize, chunk: &BytecodeChunk) -> Option<Value> {
        let jit = self.session.as_mut()?;
        let chunk_state = self.chunk_states.get_mut(func_idx)?;

        match chunk_state {
            ChunkTierState::Compiled => {
                if let Some(result) = jit.try_invoke_compiled(func_idx) {
                    return Some(Self::jit_i64_to_value(result));
                }
                *chunk_state = ChunkTierState::Interpreting;
            }
            ChunkTierState::Rejected => {
                return None;
            }
            ChunkTierState::Interpreting => {}
        }

        if self.call_hot_counts.get(func_idx).copied() == Some(0)
            && !Self::is_potentially_jittable(chunk)
        {
            *chunk_state = ChunkTierState::Rejected;
            return None;
        }

        let hot_count = self.call_hot_counts.get_mut(func_idx)?;
        *hot_count = hot_count.saturating_add(1);
        if *hot_count < JIT_HOT_CALL_THRESHOLD {
            return None;
        }

        match jit.try_compile(func_idx, chunk) {
            Ok(Some(result)) => {
                *chunk_state = ChunkTierState::Compiled;
                Some(Self::jit_i64_to_value(result))
            }
            Ok(None) | Err(_) => {
                *chunk_state = ChunkTierState::Rejected;
                None
            }
        }
    }

    #[inline(always)]
    fn is_potentially_jittable(chunk: &BytecodeChunk) -> bool {
        chunk.num_locals == 0 && chunk.handlers.is_empty() && chunk.rest_param_index.is_none()
    }

    #[inline(always)]
    fn jit_i64_to_value(result: i64) -> Value {
        Value::Int(result.clamp(i32::MIN as i64, i32::MAX as i64) as i32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::bytecode::{ConstEntry, Opcode};

    #[test]
    fn rejects_non_trivial_chunks_without_jit_attempt() {
        let mut tiering = JitTiering::new(1, true);
        let non_trivial_chunk = BytecodeChunk {
            code: vec![Opcode::LoadLocal as u8, 0, Opcode::Return as u8],
            constants: vec![],
            num_locals: 1,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };

        for _ in 0..256 {
            assert!(tiering.maybe_execute(0, &non_trivial_chunk).is_none());
        }

        assert_eq!(tiering.chunk_states[0], ChunkTierState::Rejected);
        assert_eq!(tiering.call_hot_counts[0], 0);
        let Some(session) = tiering.session.as_ref() else {
            panic!("jit session should be initialized when tiering is enabled");
        };
        assert_eq!(session.compilation_attempt_count(), 0);
    }

    #[test]
    fn compiles_trivial_chunk_once_and_uses_cache_afterwards() {
        let mut tiering = JitTiering::new(1, true);
        let trivial_chunk = BytecodeChunk {
            code: vec![Opcode::PushConst as u8, 0, Opcode::Return as u8],
            constants: vec![ConstEntry::Int(7)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };

        for _ in 0..(JIT_HOT_CALL_THRESHOLD - 1) {
            assert!(tiering.maybe_execute(0, &trivial_chunk).is_none());
        }

        let compiled_result = tiering.maybe_execute(0, &trivial_chunk);
        assert_eq!(compiled_result, Some(Value::Int(7)));
        assert_eq!(tiering.chunk_states[0], ChunkTierState::Compiled);

        let cached_result = tiering.maybe_execute(0, &trivial_chunk);
        assert_eq!(cached_result, Some(Value::Int(7)));
        let Some(session) = tiering.session.as_ref() else {
            panic!("jit session should be initialized when tiering is enabled");
        };
        assert_eq!(session.compilation_attempt_count(), 1);
    }
}
