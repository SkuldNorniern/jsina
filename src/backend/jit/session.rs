use crate::ir::bytecode::BytecodeChunk;

use super::error::BackendError;
use super::lower::bytecode_to_lamina_trivial;
use super::runtime::CompiledChunk;

enum CacheEntry {
    Unknown,
    Compiled(CompiledChunk),
    Rejected,
}

pub struct JitSession {
    cache: Vec<CacheEntry>,
    compilation_attempt_count: usize,
}

impl Default for JitSession {
    fn default() -> Self {
        Self::new()
    }
}

impl JitSession {
    pub fn new() -> Self {
        Self {
            cache: Vec::new(),
            compilation_attempt_count: 0,
        }
    }

    #[inline(always)]
    fn ensure_slot(&mut self, chunk_index: usize) {
        if chunk_index >= self.cache.len() {
            self.cache
                .resize_with(chunk_index + 1, || CacheEntry::Unknown);
        }
    }

    #[inline(always)]
    pub fn try_invoke_compiled(&self, chunk_index: usize) -> Option<i64> {
        match self.cache.get(chunk_index) {
            Some(CacheEntry::Compiled(compiled)) => Some(compiled.invoke()),
            _ => None,
        }
    }

    pub fn try_compile(
        &mut self,
        chunk_index: usize,
        chunk: &BytecodeChunk,
    ) -> Result<Option<i64>, BackendError> {
        self.ensure_slot(chunk_index);
        match &self.cache[chunk_index] {
            CacheEntry::Compiled(compiled) => return Ok(Some(compiled.invoke())),
            CacheEntry::Rejected => return Ok(None),
            CacheEntry::Unknown => {}
        }

        self.compilation_attempt_count = self.compilation_attempt_count.saturating_add(1);

        let Some(module) = bytecode_to_lamina_trivial(chunk) else {
            self.cache[chunk_index] = CacheEntry::Rejected;
            return Ok(None);
        };

        let compiled = CompiledChunk::from_module(&module)?;
        let result = compiled.invoke();
        self.cache[chunk_index] = CacheEntry::Compiled(compiled);
        Ok(Some(result))
    }

    pub fn has_compiled(&self, chunk_index: usize) -> bool {
        matches!(self.cache.get(chunk_index), Some(CacheEntry::Compiled(_)))
    }

    pub fn invoke_compiled(&self, chunk_index: usize) -> Result<i64, BackendError> {
        match self.cache.get(chunk_index) {
            Some(CacheEntry::Compiled(compiled)) => Ok(compiled.invoke()),
            Some(CacheEntry::Rejected) | Some(CacheEntry::Unknown) | None => {
                Err(BackendError::Process("chunk not compiled".to_string()))
            }
        }
    }

    pub fn compilation_attempt_count(&self) -> usize {
        self.compilation_attempt_count
    }
}
