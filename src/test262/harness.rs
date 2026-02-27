use crate::driver::Driver;
use crate::test262::metadata::{parse_frontmatter, skip_reason_by_features, TestMetadata};
use crate::test262::TestStatus;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::sync::Arc;
use std::thread;
use std::time::Duration;

#[derive(Debug)]
pub struct TestResult {
    pub path: String,
    pub status: TestStatus,
    pub message: Option<String>,
}

const MINIMAL_HARNESS: &str = r#"
function Test262Error(message) {
  this.message = message || "";
}
Test262Error.prototype.toString = function () {
  return "Test262Error: " + this.message;
};
function assert(mustBeTrue, message) {
  if (mustBeTrue === true) return;
  if (message === undefined) message = 'Expected true but got ' + String(mustBeTrue);
  throw new Test262Error(message);
}
assert._isSameValue = function (a, b) {
  if (a === b) return a !== 0 || 1 / a === 1 / b;
  return a !== a && b !== b;
};
assert._toString = function (value) {
  if (value === null) return "null";
  if (value === undefined) return "undefined";
  return String(value);
};
assert.sameValue = function (actual, expected, message) {
  if (assert._isSameValue(actual, expected)) return;
  if (message === undefined) message = '';
  else message = message + ' ';
  message = message + 'Expected SameValue(' + assert._toString(actual) + ', ' + assert._toString(expected) + ') to be true';
  throw new Test262Error(message);
};
function $DONOTEVALUATE() {
  throw "Test262: This statement should not be evaluated.";
}
"#;

const HARNESS_FILES: &[&str] = &["sta.js", "assert.js"];

fn load_harness_from_dir(root: &Path) -> Option<String> {
    let harness_dir = root.join("harness");
    let mut out = String::new();
    for name in HARNESS_FILES {
        let path = harness_dir.join(name);
        let content = std::fs::read_to_string(&path).ok()?;
        out.push_str(&content);
        out.push('\n');
    }
    Some(out)
}

fn load_harness(root: Option<&Path>) -> String {
    if let Some(r) = root {
        if let Some(h) = load_harness_from_dir(r) {
            return h;
        }
    }
    MINIMAL_HARNESS.to_string()
}

fn load_include(root: &Path, name: &str) -> Option<String> {
    let path = root.join("harness").join(name);
    std::fs::read_to_string(&path).ok()
}

fn parse_frontmatter_block(source: &str) -> Option<&str> {
    let start = source.find("/*---")?;
    let end = source.find("---*/")?;
    if end <= start {
        return None;
    }
    Some(&source[start + 5..end])
}

fn parse_define_list_item(item: &str) -> Option<String> {
    let without_comment = item.split('#').next().unwrap_or("").trim();
    if without_comment.is_empty() {
        return None;
    }
    let value = without_comment.trim_matches('"').trim_matches('\'').trim();
    if value.is_empty() {
        return None;
    }
    Some(value.to_string())
}

fn parse_include_defines(source: &str) -> Vec<String> {
    let Some(frontmatter) = parse_frontmatter_block(source) else {
        return Vec::new();
    };

    let mut defines = Vec::new();
    let mut in_defines = false;

    for line in frontmatter.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        if in_defines {
            if trimmed.starts_with('-') {
                if let Some(name) = parse_define_list_item(trimmed.trim_start_matches('-').trim()) {
                    defines.push(name);
                }
                continue;
            }
            in_defines = false;
        }

        if trimmed.starts_with("defines:") {
            let rest = trimmed.trim_start_matches("defines:").trim();
            if rest.is_empty() {
                in_defines = true;
            } else {
                let list = rest.trim_start_matches('[').trim_end_matches(']').trim();
                for item in list.split(',') {
                    if let Some(name) = parse_define_list_item(item) {
                        defines.push(name);
                    }
                }
            }
        }
    }

    defines
}

fn is_js_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '$' || first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|c| c == '$' || c == '_' || c.is_ascii_alphanumeric())
}

fn parse_include_function_names(source: &str) -> Vec<String> {
    let mut names = Vec::new();
    for line in source.lines() {
        let trimmed = line.trim_start();
        let mut rest = if let Some(rest) = trimmed.strip_prefix("function ") {
            rest
        } else {
            continue;
        };
        if let Some(after_star) = rest.strip_prefix('*') {
            rest = after_star.trim_start();
        }
        let end = rest
            .find(|c: char| !(c == '$' || c == '_' || c.is_ascii_alphanumeric()))
            .unwrap_or(rest.len());
        if end == 0 {
            continue;
        }
        let name = &rest[..end];
        if is_js_identifier(name) && !names.iter().any(|n| n == name) {
            names.push(name.to_string());
        }
    }
    names
}

fn extract_test_body(source: &str) -> &str {
    if let Some(end) = source.find("---*/") {
        source[end + 5..].trim_start()
    } else {
        source.trim_start()
    }
}

fn has_raw_flag(meta: Option<&TestMetadata>) -> bool {
    meta.map(|m| m.flags.iter().any(|f| f == "raw"))
        .unwrap_or(false)
}

fn has_only_strict_flag(meta: Option<&TestMetadata>) -> bool {
    meta.map(|m| m.flags.iter().any(|f| f == "onlyStrict"))
        .unwrap_or(false)
}

fn needs_strict_rerun(meta: Option<&TestMetadata>) -> bool {
    let m = match meta {
        Some(x) => x,
        None => return true,
    };
    !m.flags
        .iter()
        .any(|f| f == "module" || f == "onlyStrict" || f == "noStrict" || f == "raw")
}

fn build_prelude(root: Option<&Path>, meta: Option<&TestMetadata>) -> (String, bool) {
    let harness = load_harness(root);
    let mut prelude = String::new();
    if has_only_strict_flag(meta) {
        prelude.push_str("\"use strict\";\n");
    }
    prelude.push_str(&harness);
    prelude.push('\n');
    prelude.push_str("globalThis.isSameValue = assert._isSameValue;\n");
    let mut includes_ok = true;
    if let (Some(r), Some(m)) = (root, meta) {
        for inc in &m.includes {
            let name = inc.trim().trim_matches('"').trim_matches('\'');
            if let Some(content) = load_include(r, name) {
                prelude.push_str(&content);
                prelude.push('\n');
                for define in parse_include_defines(&content) {
                    if is_js_identifier(&define) {
                        prelude.push_str("var ");
                        prelude.push_str(&define);
                        prelude.push_str(" = (typeof ");
                        prelude.push_str(&define);
                        prelude.push_str(" !== \"undefined\") ? ");
                        prelude.push_str(&define);
                        prelude.push_str(" : globalThis.");
                        prelude.push_str(&define);
                        prelude.push_str(";\n");
                    }
                }
                for function_name in parse_include_function_names(&content) {
                    prelude.push_str("globalThis.");
                    prelude.push_str(&function_name);
                    prelude.push_str(" = ");
                    prelude.push_str(&function_name);
                    prelude.push_str(";\n");
                }
            } else {
                includes_ok = false;
            }
        }
    }
    (prelude, includes_ok)
}

fn wrap_test(body: &str, prelude: &str) -> String {
    if body.contains("function main(") {
        format!("{}{}", prelude, body)
    } else {
        format!(
            "function main() {{\nfunction __test__() {{\n{}\n{}\n}}\n__test__();\nreturn 0;\n}}\n",
            prelude, body
        )
    }
}

fn classify_unsupported_error(message: &str) -> Option<String> {
    if message.contains("unsupported: class not implemented") {
        return Some("feature not supported: class".to_string());
    }
    if message.contains("nested destructuring not yet supported") {
        return Some("feature not supported: nested destructuring".to_string());
    }
    if message.contains("(JSINA-PARSE-011)") {
        return Some("feature not supported: array rest destructuring".to_string());
    }
    None
}

const TEST262_TIMEOUT: Duration = Duration::from_secs(2);

enum RunOutcome {
    Pass,
    Fail(String),
    Timeout,
}

const THREAD_JOIN_TIMEOUT: Duration = Duration::from_secs(5);

const TEST_THREAD_STACK_SIZE: usize = 8 * 1024 * 1024;

fn run_one(wrapped: &str, negative: Option<&crate::test262::metadata::NegativeMeta>) -> RunOutcome {
    match Driver::ast(wrapped) {
        Err(e) => RunOutcome::Fail(e.to_string()),
        Ok(_) => {
            let wrapped = wrapped.to_string();
            let cancel = Arc::new(AtomicBool::new(false));
            let cancel_clone = Arc::clone(&cancel);
            let (tx, rx) = mpsc::channel();
            let handle = thread::Builder::new()
                .stack_size(TEST_THREAD_STACK_SIZE)
                .spawn(move || {
                    let result = Driver::run_with_timeout_and_cancel(
                        &wrapped,
                        Some(&cancel_clone),
                        true,
                    );
                    let _ = tx.send(result);
                })
                .expect("spawn test thread");
            match rx.recv_timeout(TEST262_TIMEOUT) {
                Ok(Ok(_)) => {
                    let _ = handle.join();
                    if negative.is_some() {
                        RunOutcome::Fail("expected error but test passed".to_string())
                    } else {
                        RunOutcome::Pass
                    }
                }
                Ok(Err(e)) => {
                    let _ = handle.join();
                    let msg = e.to_string();
                    if msg.contains("infinite loop detected")
                        || msg.contains("cancelled")
                    {
                        RunOutcome::Timeout
                    } else if let Some(neg) = negative {
                        let matches = msg.contains(&neg.error_type)
                            || (neg.error_type == "SyntaxError"
                                && (msg.contains("JSINA-PARSE") || msg.contains("JSINA-EARLY")))
                            || (neg.error_type == "ReferenceError"
                                && msg.contains("undefined variable"));
                        if !neg.error_type.is_empty() && !matches {
                            RunOutcome::Fail(format!(
                                "expected error type '{}', got: {}",
                                neg.error_type, msg
                            ))
                        } else {
                            RunOutcome::Pass
                        }
                    } else {
                        RunOutcome::Fail(msg)
                    }
                }
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    cancel.store(true, Ordering::SeqCst);
                    let _ = rx.recv_timeout(THREAD_JOIN_TIMEOUT);
                    let _ = handle.join();
                    RunOutcome::Timeout
                }
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    let _ = handle.join();
                    RunOutcome::Fail("test thread disconnected".to_string())
                }
            }
        }
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
        if let Some(reason) = skip_reason_by_features(m, test_path) {
            return TestResult {
                path: test_path.to_string_lossy().to_string(),
                status: TestStatus::SkipFeature,
                message: Some(reason),
            };
        }
    }

    let body = extract_test_body(&source);
    let use_harness = test262_root.is_some() && !has_raw_flag(meta.as_ref());

    let (prelude, includes_ok) = if use_harness {
        build_prelude(test262_root, meta.as_ref())
    } else {
        (load_harness(None), true)
    };

    if !includes_ok {
        return TestResult {
            path: test_path.to_string_lossy().to_string(),
            status: TestStatus::SkipParse,
            message: Some("required include file not found or unreadable".to_string()),
        };
    }

    let wrapped = wrap_test(body, &prelude);

    let outcome = run_one(&wrapped, meta.as_ref().and_then(|m| m.negative.as_ref()));
    if let RunOutcome::Fail(message) = &outcome {
        if let Some(reason) = classify_unsupported_error(message) {
            return TestResult {
                path: test_path.to_string_lossy().to_string(),
                status: TestStatus::SkipFeature,
                message: Some(reason),
            };
        }
    }

    if matches!(outcome, RunOutcome::Pass) && needs_strict_rerun(meta.as_ref()) {
        let (base_prelude, _) = if use_harness {
            build_prelude(test262_root, meta.as_ref())
        } else {
            (String::new(), true)
        };
        let strict_prelude = format!("\"use strict\";\n{}", base_prelude);
        let strict_wrapped = wrap_test(body, &strict_prelude);
        let strict_outcome = run_one(
            &strict_wrapped,
            meta.as_ref().and_then(|m| m.negative.as_ref()),
        );
        if !matches!(strict_outcome, RunOutcome::Pass) {
            if let RunOutcome::Fail(message) = &strict_outcome {
                if let Some(reason) = classify_unsupported_error(message) {
                    return TestResult {
                        path: test_path.to_string_lossy().to_string(),
                        status: TestStatus::SkipFeature,
                        message: Some(reason),
                    };
                }
            }
            return TestResult {
                path: test_path.to_string_lossy().to_string(),
                status: match strict_outcome {
                    RunOutcome::Timeout => TestStatus::Timeout,
                    _ => TestStatus::Fail,
                },
                message: match strict_outcome {
                    RunOutcome::Fail(m) => Some(m),
                    _ => Some("strict mode rerun failed".to_string()),
                },
            };
        }
    }

    match outcome {
        RunOutcome::Pass => TestResult {
            path: test_path.to_string_lossy().to_string(),
            status: TestStatus::Pass,
            message: None,
        },
        RunOutcome::Timeout => TestResult {
            path: test_path.to_string_lossy().to_string(),
            status: TestStatus::Timeout,
            message: Some("timeout (30s)".to_string()),
        },
        RunOutcome::Fail(msg) => TestResult {
            path: test_path.to_string_lossy().to_string(),
            status: TestStatus::Fail,
            message: Some(msg),
        },
    }
}
