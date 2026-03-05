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

    #[derive(Clone, Copy)]
    enum ActiveList {
        Features,
        Flags,
        Includes,
    }

    fn push_list_item(meta: &mut TestMetadata, list: ActiveList, item: &str) {
        let value = item.trim().trim_matches('"').trim_matches('\'');
        if value.is_empty() {
            return;
        }
        match list {
            ActiveList::Features => meta.features.push(value.to_string()),
            ActiveList::Flags => meta.flags.push(value.to_string()),
            ActiveList::Includes => meta.includes.push(value.to_string()),
        }
    }

    let mut active_list: Option<ActiveList> = None;

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
            } else if !line.starts_with(' ') && !trimmed.starts_with('-') {
                in_negative = false;
                if !negative_phase.is_empty() && !negative_type.is_empty() {
                    meta.negative = Some(NegativeMeta {
                        phase: negative_phase.clone(),
                        error_type: negative_type.clone(),
                    });
                }
            }
        }

        if let Some(list) = active_list {
            if trimmed.starts_with('-') {
                push_list_item(&mut meta, list, trimmed.trim_start_matches('-').trim());
                continue;
            }
            active_list = None;
        }

        if trimmed.starts_with("negative:") {
            in_negative = true;
            negative_phase.clear();
            negative_type.clear();
            active_list = None;
        } else if trimmed.starts_with("description:") {
            meta.description = Some(
                trimmed
                    .trim_start_matches("description:")
                    .trim()
                    .to_string(),
            );
        } else if trimmed.starts_with("features:") {
            let rest = trimmed.trim_start_matches("features:");
            if rest.trim().is_empty() {
                active_list = Some(ActiveList::Features);
            } else {
                meta.features = parse_yaml_list(rest);
            }
        } else if trimmed.starts_with("flags:") {
            let rest = trimmed.trim_start_matches("flags:");
            if rest.trim().is_empty() {
                active_list = Some(ActiveList::Flags);
            } else {
                meta.flags = parse_yaml_list(rest);
            }
        } else if trimmed.starts_with("includes:") {
            let rest = trimmed.trim_start_matches("includes:");
            if rest.trim().is_empty() {
                active_list = Some(ActiveList::Includes);
            } else {
                meta.includes = parse_yaml_list(rest);
            }
        }
    }
    if in_negative {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_multiline_includes_list() {
        let yaml = r#"
includes:
  - propertyHelper.js
  - compareArray.js
"#;
        let meta = parse_yaml_frontmatter(yaml).expect("frontmatter");
        assert_eq!(meta.includes, vec!["propertyHelper.js", "compareArray.js"]);
    }

    #[test]
    fn parse_multiline_features_and_flags() {
        let yaml = r#"
features:
  - class
flags:
  - module
"#;
        let meta = parse_yaml_frontmatter(yaml).expect("frontmatter");
        assert_eq!(meta.features, vec!["class"]);
        assert_eq!(meta.flags, vec!["module"]);
    }
}
