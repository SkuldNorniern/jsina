fn is_line_terminator(ch: char) -> bool {
    matches!(ch, '\n' | '\r' | '\u{2028}' | '\u{2029}')
}

pub fn normalize_function_constructor_source(
    source: &str,
    line_start_initial: bool,
    replacement: &str,
) -> String {
    let mut normalized = String::with_capacity(source.len());
    let mut byte_index = 0;
    let mut at_line_start = line_start_initial;

    while byte_index < source.len() {
        let rest = &source[byte_index..];

        if rest.starts_with("<!--") {
            normalized.push_str(replacement);
            byte_index += 4;
            at_line_start = false;
            continue;
        }

        if at_line_start && rest.starts_with("-->") {
            normalized.push_str(replacement);
            byte_index += 3;
            at_line_start = false;
            continue;
        }

        if let Some(ch) = rest.chars().next() {
            normalized.push(ch);
            byte_index += ch.len_utf8();
            if is_line_terminator(ch) {
                at_line_start = true;
            } else if at_line_start && matches!(ch, ' ' | '\t') {
                at_line_start = true;
            } else {
                at_line_start = false;
            }
        } else {
            break;
        }
    }

    normalized
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_rewrites_open_and_close_markers() {
        let source = "<!--a\n  -->b";
        let normalized = normalize_function_constructor_source(source, true, "//");
        assert_eq!(normalized, "//a\n  //b");
    }
}
