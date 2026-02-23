mod harness;
mod metadata;

pub use harness::{run_test, TestResult};
pub use metadata::{parse_frontmatter, TestMetadata};

use std::path::Path;

pub const ALLOWLIST_PATH: &str = "test262/allowlist.txt";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestStatus {
    Pass,
    Fail,
    SkipFeature,
    SkipParse,
    HarnessError,
}

pub fn load_allowlist(path: &Path) -> Vec<AllowlistEntry> {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };
    content
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                return None;
            }
            let parts: Vec<&str> = line.split('|').map(|s| s.trim()).collect();
            if parts.is_empty() || parts[0].is_empty() {
                return None;
            }
            Some(AllowlistEntry {
                test_path: parts[0].to_string(),
                reason: parts.get(1).copied().unwrap_or("").to_string(),
                owner: parts.get(2).copied().unwrap_or("").to_string(),
                date: parts.get(3).copied().unwrap_or("").to_string(),
            })
        })
        .collect()
}

#[derive(Debug, Clone)]
pub struct AllowlistEntry {
    pub test_path: String,
    pub reason: String,
    pub owner: String,
    pub date: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn allowlist_parse() {
        let p = Path::new("test262/allowlist.txt");
        let entries = load_allowlist(p);
        assert!(
            !entries.is_empty(),
            "allowlist should have at least one entry for bootstrap"
        );
    }

    #[test]
    fn run_corpus_test() {
        let result = run_test(
            Path::new("tests/test262/corpus/simple-return.js"),
            None,
        );
        assert_eq!(result.status, TestStatus::Pass, "corpus test should pass");
    }
}
