#[derive(Debug, Default)]
pub struct TestMetadata {
    pub description: Option<String>,
    pub negative: Option<NegativeMeta>,
    pub features: Vec<String>,
    pub flags: Vec<String>,
    pub includes: Vec<String>,
}

#[derive(Debug)]
pub struct NegativeMeta {
    pub phase: String,
    pub error_type: String,
}

const FEATURES_WE_SKIP: &[&str] = &[
    "async-iteration",
    "AsyncIterator",
    "BigInt",
    "class",
    "class-fields-private",
    "class-fields-public",
    "class-static-fields-private",
    "class-static-fields-public",
    "dynamic-import",
    "import.meta",
    "json-modules",
    "logical-assignment",
    "numeric-separator-literal",
    "object-rest",
    "object-spread",
    "optional-catch-binding",
    "optional-chaining",
    "regexp-dotall",
    "regexp-match-indices",
    "regexp-named-groups",
    "regexp-unicode-property-escapes",
    "shadowrealm",
    "String.prototype.replaceAll",
    "tail-call-optimization",
    "top-level-await",
    "WeakRef",
    "decorators",
    "iterator-helpers",
    "explicit-resource-management",
];

pub fn parse_frontmatter(source: &str) -> Option<TestMetadata> {
    let start = source.find("/*---")?;
    let end = source.find("---*/")?;
    if end <= start {
        return None;
    }
    let yaml = &source[start + 5..end];
    parse_yaml_frontmatter(yaml)
}

fn parse_yaml_frontmatter(yaml: &str) -> Option<TestMetadata> {
    let mut meta = TestMetadata::default();
    let mut in_negative = false;
    let mut negative_phase = String::new();
    let mut negative_type = String::new();

    for line in yaml.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if in_negative {
            if trimmed.starts_with("phase:") {
                negative_phase = trimmed.trim_start_matches("phase:").trim().to_string();
            } else if trimmed.starts_with("type:") {
                negative_type = trimmed.trim_start_matches("type:").trim().to_string();
            } else if !trimmed.starts_with(' ') && !trimmed.starts_with('-') {
                in_negative = false;
                if !negative_phase.is_empty() && !negative_type.is_empty() {
                    meta.negative = Some(NegativeMeta {
                        phase: negative_phase.clone(),
                        error_type: negative_type.clone(),
                    });
                }
            }
        }
        if trimmed.starts_with("negative:") {
            in_negative = true;
            negative_phase.clear();
            negative_type.clear();
        } else if trimmed.starts_with("description:") {
            meta.description = Some(trimmed.trim_start_matches("description:").trim().to_string());
        } else if trimmed.starts_with("features:") {
            let rest = trimmed.trim_start_matches("features:");
            meta.features = parse_yaml_list(rest);
        } else if trimmed.starts_with("flags:") {
            let rest = trimmed.trim_start_matches("flags:");
            meta.flags = parse_yaml_list(rest);
        } else if trimmed.starts_with("includes:") {
            let rest = trimmed.trim_start_matches("includes:");
            meta.includes = parse_yaml_list(rest);
        }
    }
    if in_negative && !negative_phase.is_empty() && !negative_type.is_empty() {
        meta.negative = Some(NegativeMeta {
            phase: negative_phase,
            error_type: negative_type,
        });
    }
    Some(meta)
}

fn parse_yaml_list(s: &str) -> Vec<String> {
    let s = s.trim();
    let s = s.trim_start_matches('[').trim_end_matches(']').trim();
    s.split(',')
        .map(|x| x.trim().trim_matches('"').trim_matches('\'').to_string())
        .filter(|x| !x.is_empty())
        .collect()
}

pub fn should_skip_by_features(meta: &TestMetadata) -> bool {
    for f in &meta.features {
        if FEATURES_WE_SKIP.iter().any(|s| f.contains(s)) {
            return true;
        }
    }
    if meta.flags.iter().any(|f| f == "module" || f == "async") {
        return true;
    }
    if !meta.includes.is_empty() {
        return true;
    }
    false
}
