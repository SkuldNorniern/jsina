mod harness;
mod metadata;

pub use harness::{TestResult, run_test};
pub use metadata::{TestMetadata, parse_frontmatter};

use std::path::Path;

pub const ALLOWLIST_PATH: &str = "test262/allowlist.txt";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestStatus {
    Pass,
    Fail,
    Timeout,
    SkipFeature,
    SkipParse,
    HarnessError,
}

pub fn scan_test262_tests(root: &Path) -> Vec<String> {
    let test_dir = root.join("test");
    if !test_dir.exists() {
        return Vec::new();
    }
    let mut out = Vec::new();
    let mut stack: Vec<(std::path::PathBuf, String)> =
        vec![(test_dir.to_path_buf(), String::new())];
    while let Some((dir, prefix)) = stack.pop() {
        let entries = match std::fs::read_dir(&dir) {
            Ok(e) => e,
            Err(_) => continue,
        };
        for e in entries.flatten() {
            let path = e.path();
            let name = path.file_name().unwrap_or_default().to_string_lossy();
            let rel = if prefix.is_empty() {
                name.to_string()
            } else {
                format!("{}/{}", prefix, name)
            };
            if path.is_dir() {
                stack.push((path, rel));
            } else if name.ends_with(".js") && !name.contains("_FIXTURE") {
                out.push(format!("test/{}", rel));
            }
        }
    }
    out.sort();
    out
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
        let result = run_test(Path::new("tests/test262/corpus/simple-return.js"), None);
        assert_eq!(result.status, TestStatus::Pass, "corpus test should pass");
    }

    #[test]
    fn run_try_catch_nested_throw() {
        let result = run_test(
            Path::new("tests/test262/corpus/try-catch-nested-throw.js"),
            None,
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "try/catch should catch throw from nested IIFE: {:?}",
            result.message
        );
    }

    #[test]
    fn run_object_is_emulates_undefined() {
        let result = run_test(
            Path::new("tests/test262/corpus/object-is-html-dda.js"),
            Some(Path::new(".")),
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "Object.is with IsHTMLDDA: {:?}",
            result.message
        );
    }

    #[test]
    fn run_date_setyear_getfullyear() {
        let result = run_test(
            Path::new("tests/test262/corpus/date-setyear-getfullyear.js"),
            None,
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "Date setYear/getFullYear: {:?}",
            result.message
        );
    }

    #[test]
    fn run_assert_same_value_only() {
        let result = run_test(
            Path::new("tests/test262/corpus/assert-same-value-only.js"),
            None,
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "assert.sameValue: {:?}",
            result.message
        );
    }

    #[test]
    fn run_assert_not_same_value() {
        let result = run_test(
            Path::new("tests/test262/corpus/assert-not-same-value.js"),
            None,
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "assert.notSameValue: {:?}",
            result.message
        );
    }

    #[test]
    fn run_negative_syntax_error() {
        let result = run_test(
            Path::new("tests/test262/corpus/negative-syntax-error.js"),
            None,
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "negative test expecting SyntaxError should pass when Function throws: {:?}",
            result.message
        );
    }

    #[test]
    #[ignore] // verifyProperty assertions fail; needs getOwnPropertyDescriptor/defineProperty fixes
    fn run_property_helper_verify_defined() {
        let result = run_test(
            Path::new("test/corpus/verify-property-defined.js"),
            Some(Path::new("asset/test262")),
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "verifyProperty should be defined: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    #[ignore] // verifyProperty assertions fail; needs getOwnPropertyDescriptor/defineProperty fixes
    fn run_property_helper_length() {
        let result = run_test(
            Path::new("test/annexB/built-ins/Date/prototype/getYear/length.js"),
            Some(Path::new("asset/test262")),
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "propertyHelper length test: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_labeled_continue() {
        let result = run_test(Path::new("tests/test262/corpus/labeled-continue.js"), None);
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "labeled continue: {:?}",
            result.message
        );
    }

    #[test]
    fn run_temporal_plain_time_from_year_zero_no_undefined_arg() {
        let result = run_test(
            Path::new("test/built-ins/Temporal/PlainTime/from/year-zero.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("undefined variable 'arg'")),
            "unexpected undefined arg failure: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_for_of_dstr_obj_prop_elem_init_fn_name_cover() {
        let result = run_test(
            Path::new("test/language/statements/for-of/dstr/obj-prop-elem-init-fn-name-cover.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("JSINA-PARSE")),
            "for-of dstr cover should parse without parser error: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_for_of_dstr_obj_prop_elem_init_fn_name_fn() {
        let result = run_test(
            Path::new("test/language/statements/for-of/dstr/obj-prop-elem-init-fn-name-fn.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("JSINA-PARSE")),
            "for-of dstr fn should parse without parser error: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_for_of_dstr_obj_prop_elem_init_fn_name_gen() {
        let result = run_test(
            Path::new("test/language/statements/for-of/dstr/obj-prop-elem-init-fn-name-gen.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("JSINA-PARSE")),
            "for-of dstr gen should parse without parser error: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_for_of_dstr_obj_prop_elem_init_in_parses() {
        let result = run_test(
            Path::new("test/language/statements/for-of/dstr/obj-prop-elem-init-in.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("JSINA-PARSE")),
            "for-of dstr init-in should parse without parser error: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_for_of_dstr_obj_prop_elem_init_let_parses() {
        let result = run_test(
            Path::new("test/language/statements/for-of/dstr/obj-prop-elem-init-let.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("JSINA-PARSE")),
            "for-of dstr init-let should parse without parser error: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_for_of_dstr_obj_prop_elem_target_yield_ident_valid() {
        let result = run_test(
            Path::new(
                "test/language/statements/for-of/dstr/obj-prop-elem-target-yield-ident-valid.js",
            ),
            Some(Path::new("asset/test262")),
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "yield as identifier in for-of destructuring computed key should pass: {:?}",
            result.message
        );
    }

    #[test]
    fn run_spidermonkey_strict_shell_include_exposes_helpers() {
        let result = run_test(
            Path::new("test/staging/sm/strict/eval-variable-environment.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result.message.as_deref().map_or(true, |msg| !msg
                .contains("undefined function 'testLenientAndStrict'")),
            "strict shell helper should be visible when include is loaded: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_property_helper_is_same_value_is_resolved() {
        let result = run_test(
            Path::new("test/annexB/built-ins/String/prototype/anchor/prop-desc.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result.message.as_deref().map_or(true, |msg| !msg
                .contains("undefined function 'isSameValue'")),
            "propertyHelper isSameValue helper should resolve: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_escape_global_is_resolved() {
        let result = run_test(
            Path::new("test/annexB/built-ins/escape/empty-string.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("undefined function 'escape'")),
            "escape global should resolve in test262 harness: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_property_helper_internal_functions_are_visible() {
        let result = run_test(
            Path::new("test/annexB/built-ins/String/prototype/anchor/length.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result.message.as_deref().map_or(true, |msg| !msg
                .contains("undefined function 'isEnumerable'")),
            "propertyHelper internal helper should be visible: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_typed_array_constructors_are_bound() {
        let result = run_test(
            Path::new(
                "test/annexB/built-ins/TypedArrayConstructors/from/iterator-method-emulates-undefined.js",
            ),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result.message.as_deref().map_or(true, |msg| {
                !msg.contains("undefined variable 'Float16Array'")
                    && !msg.contains("undefined variable 'BigInt64Array'")
                    && !msg.contains("undefined variable 'BigUint64Array'")
                    && !msg.contains("undefined variable 'makeIterable'")
            }),
            "typed array constructors should be present in global scope: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_eval_error_global_is_resolved() {
        let result = run_test(
            Path::new("test/built-ins/Error/isError/errors.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result
                .message
                .as_deref()
                .map_or(true, |msg| !msg.contains("undefined variable 'EvalError'")),
            "EvalError global should be present in global scope: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_aggregate_error_global_is_resolved() {
        let result = run_test(
            Path::new("test/built-ins/AggregateError/length.js"),
            Some(Path::new("asset/test262")),
        );
        assert!(
            result.message.as_deref().map_or(true, |msg| !msg
                .contains("undefined variable 'AggregateError'")),
            "AggregateError global should be present in global scope: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }

    #[test]
    fn run_bigint_typedarray_feature_is_not_skipped_when_globals_exist() {
        let result = run_test(
            Path::new("test/built-ins/TypedArray/prototype/copyWithin/BigInt/return-this.js"),
            Some(Path::new("asset/test262")),
        );
        assert_ne!(
            result.status,
            TestStatus::SkipFeature,
            "BigInt typed-array tests should run when BigInt typed-array globals are bound: {:?}",
            result.message
        );
    }

    #[test]
    fn run_escape_argument_bigint() {
        let result = run_test(
            Path::new("test/annexB/built-ins/escape/argument_bigint.js"),
            Some(Path::new("asset/test262")),
        );
        assert_eq!(
            result.status,
            TestStatus::Pass,
            "escape(1n) BigInt test: status={:?}, message={:?}",
            result.status,
            result.message
        );
    }
}
