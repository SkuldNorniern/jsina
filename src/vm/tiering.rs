use crate::backend::JitSession;
use crate::ir::bytecode::BytecodeChunk;
use crate::runtime::Value;

const DEFAULT_JIT_HOT_CALL_THRESHOLD: u32 = 16;

#[derive(Clone, Copy, Debug, Default)]
pub struct JitTieringStats {
    pub hot_call_threshold: u32,
    pub jit_invocations: u64,
    pub compile_attempts: u64,
    pub compile_successes: u64,
    pub compile_rejections: u64,
    pub precheck_rejections: u64,
    pub compiled_chunk_count: usize,
    pub rejected_chunk_count: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ChunkTierState {
    Interpreting,
    Compiled,
    Rejected,
}

pub struct JitTiering {
    session: Option<JitSession>,
    hot_call_threshold: u32,
    call_hot_counts: Vec<u32>,
    chunk_states: Vec<ChunkTierState>,
    stats: JitTieringStats,
}

impl JitTiering {
    pub fn new(program_chunk_count: usize, enabled: bool) -> Self {
        let hot_call_threshold = configured_hot_call_threshold();
        Self {
            session: if enabled {
                Some(JitSession::new())
            } else {
                None
            },
            hot_call_threshold,
            call_hot_counts: vec![0; program_chunk_count],
            chunk_states: vec![ChunkTierState::Interpreting; program_chunk_count],
            stats: JitTieringStats {
                hot_call_threshold,
                ..JitTieringStats::default()
            },
        }
    }

    #[inline(always)]
    pub fn maybe_execute(&mut self, func_idx: usize, chunk: &BytecodeChunk) -> Option<Value> {
        let jit = self.session.as_mut()?;
        let chunk_state = self.chunk_states.get_mut(func_idx)?;

        match chunk_state {
            ChunkTierState::Compiled => {
                if let Some(result) = jit.try_invoke_compiled(func_idx) {
                    self.stats.jit_invocations = self.stats.jit_invocations.saturating_add(1);
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
            self.stats.precheck_rejections = self.stats.precheck_rejections.saturating_add(1);
            return None;
        }

        let hot_count = self.call_hot_counts.get_mut(func_idx)?;
        *hot_count = hot_count.saturating_add(1);
        if *hot_count < self.hot_call_threshold {
            return None;
        }

        self.stats.compile_attempts = self.stats.compile_attempts.saturating_add(1);
        match jit.try_compile(func_idx, chunk) {
            Ok(Some(result)) => {
                *chunk_state = ChunkTierState::Compiled;
                self.stats.compile_successes = self.stats.compile_successes.saturating_add(1);
                Some(Self::jit_i64_to_value(result))
            }
            Ok(None) | Err(_) => {
                *chunk_state = ChunkTierState::Rejected;
                self.stats.compile_rejections = self.stats.compile_rejections.saturating_add(1);
                None
            }
        }
    }

    pub fn stats(&self) -> JitTieringStats {
        let mut snapshot = self.stats;
        snapshot.compiled_chunk_count = self
            .chunk_states
            .iter()
            .filter(|state| matches!(state, ChunkTierState::Compiled))
            .count();
        snapshot.rejected_chunk_count = self
            .chunk_states
            .iter()
            .filter(|state| matches!(state, ChunkTierState::Rejected))
            .count();
        snapshot
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

fn configured_hot_call_threshold() -> u32 {
    std::env::var("JSINA_JIT_HOT_THRESHOLD")
        .ok()
        .and_then(|raw| raw.parse::<u32>().ok())
        .filter(|threshold| *threshold > 0)
        .unwrap_or(DEFAULT_JIT_HOT_CALL_THRESHOLD)
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

        for _ in 0..(tiering.hot_call_threshold - 1) {
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
