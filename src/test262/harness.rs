use crate::driver::Driver;
use crate::test262::metadata::{parse_frontmatter, should_skip_by_features};
use crate::test262::TestStatus;
use std::path::Path;

#[derive(Debug)]
pub struct TestResult {
    pub path: String,
    pub status: TestStatus,
    pub message: Option<String>,
}

fn extract_test_body(source: &str) -> &str {
    if let Some(end) = source.find("---*/") {
        source[end + 5..].trim_start()
    } else {
        source.trim_start()
    }
}

fn wrap_test(body: &str) -> String {
    if body.contains("function main(") {
        body.to_string()
    } else {
        format!(
            "function __test__() {{\n{}\n}}\nfunction main() {{\n  __test__();\n  return 0;\n}}\n",
            body
        )
    }
}

pub fn run_test(test_path: &Path, test262_root: Option<&Path>) -> TestResult {
    let full_path = if test_path.is_absolute() || test_path.exists() {
        test_path.to_path_buf()
    } else if let Some(root) = test262_root {
        root.join(test_path)
    } else {
        test_path.to_path_buf()
    };
    let source = match std::fs::read_to_string(&full_path) {
        Ok(s) => s,
        Err(e) => {
            return TestResult {
                path: test_path.to_string_lossy().to_string(),
                status: TestStatus::HarnessError,
                message: Some(format!("read error: {}", e)),
            };
        }
    };

    let meta = parse_frontmatter(&source);
    if let Some(ref m) = meta {
        if should_skip_by_features(m) {
            return TestResult {
                path: test_path.to_string_lossy().to_string(),
                status: TestStatus::SkipFeature,
                message: None,
            };
        }
    }

    let body = extract_test_body(&source);
    let wrapped = wrap_test(body);

    match Driver::ast(&wrapped) {
        Err(_) => TestResult {
            path: test_path.to_string_lossy().to_string(),
            status: TestStatus::SkipParse,
            message: None,
        },
        Ok(_) => match Driver::run(&wrapped) {
            Ok(_) => TestResult {
                path: test_path.to_string_lossy().to_string(),
                status: TestStatus::Pass,
                message: None,
            },
            Err(e) => TestResult {
                path: test_path.to_string_lossy().to_string(),
                status: TestStatus::Fail,
                message: Some(e.to_string()),
            },
        },
    }
}
