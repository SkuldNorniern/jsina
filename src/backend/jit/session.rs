use std::collections::HashMap;

use crate::ir::bytecode::BytecodeChunk;

use super::error::BackendError;
use super::lower::bytecode_to_lamina_trivial;
use super::runtime::CompiledChunk;

enum CacheEntry {
    Compiled(CompiledChunk),
    Rejected,
}

pub struct JitSession {
    cache: HashMap<usize, CacheEntry>,
}

impl Default for JitSession {
    fn default() -> Self {
        Self::new()
    }
}

impl JitSession {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    pub fn try_invoke_compiled(&self, chunk_index: usize) -> Option<i64> {
        match self.cache.get(&chunk_index) {
            Some(CacheEntry::Compiled(compiled)) => Some(compiled.invoke()),
            _ => None,
        }
    }

    pub fn try_compile(
        &mut self,
        chunk_index: usize,
        chunk: &BytecodeChunk,
    ) -> Result<Option<i64>, BackendError> {
        if let Some(entry) = self.cache.get(&chunk_index) {
            return match entry {
                CacheEntry::Compiled(compiled) => Ok(Some(compiled.invoke())),
                CacheEntry::Rejected => Ok(None),
            };
        }

        let Some(module) = bytecode_to_lamina_trivial(chunk) else {
            self.cache.insert(chunk_index, CacheEntry::Rejected);
            return Ok(None);
        };

        let compiled = CompiledChunk::from_module(&module)?;
        let result = compiled.invoke();
        self.cache
            .insert(chunk_index, CacheEntry::Compiled(compiled));
        Ok(Some(result))
    }

    pub fn has_compiled(&self, chunk_index: usize) -> bool {
        matches!(self.cache.get(&chunk_index), Some(CacheEntry::Compiled(_)))
    }

    pub fn invoke_compiled(&self, chunk_index: usize) -> Result<i64, BackendError> {
        match self.cache.get(&chunk_index) {
            Some(CacheEntry::Compiled(compiled)) => Ok(compiled.invoke()),
            Some(CacheEntry::Rejected) | None => {
                Err(BackendError::Process("chunk not compiled".to_string()))
            }
        }
    }
}
