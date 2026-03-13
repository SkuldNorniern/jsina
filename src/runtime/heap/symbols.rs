use super::Heap;

impl Heap {
    pub fn symbol_for(&mut self, key: &str) -> usize {
        if let Some(&symbol_id) = self.symbol_for_registry.get(key) {
            return symbol_id;
        }
        let symbol_id = self.alloc_symbol(Some(key.to_string()));
        self.symbol_for_registry.insert(key.to_string(), symbol_id);
        self.symbol_key_registry.insert(symbol_id, key.to_string());
        symbol_id
    }

    pub fn symbol_key_for(&self, symbol_id: usize) -> Option<&str> {
        self.symbol_key_registry
            .get(&symbol_id)
            .map(|key| key.as_str())
    }

    pub fn alloc_symbol(&mut self, description: Option<String>) -> usize {
        let symbol_id = self.symbols.len();
        self.symbols.push(description);
        symbol_id
    }

    pub fn symbol_description(&self, symbol_id: usize) -> Option<&str> {
        self.symbols
            .get(symbol_id)
            .and_then(|description| description.as_deref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol_for_registers_reverse_lookup() {
        let mut heap = Heap::new();
        let symbol_id = heap.symbol_for("alpha");
        assert_eq!(heap.symbol_key_for(symbol_id), Some("alpha"));
    }

    #[test]
    fn symbol_key_for_ignores_non_registry_symbols() {
        let mut heap = Heap::new();
        let symbol_id = heap.alloc_symbol(Some("local".to_string()));
        assert_eq!(heap.symbol_key_for(symbol_id), None);
    }
}
