pub(super) fn is_line_terminator(ch: char) -> bool {
    matches!(ch, '\n' | '\r' | '\u{2028}' | '\u{2029}')
}

pub(super) fn is_horizontal_whitespace(ch: char) -> bool {
    ch.is_whitespace() && !is_line_terminator(ch)
}

fn char_at(source: &str, byte_offset: usize, index: usize) -> Option<char> {
    source
        .get(byte_offset..)
        .and_then(|rest| rest.chars().nth(index))
}

pub(super) fn starts_html_open_comment(source: &str, byte_offset: usize) -> bool {
    char_at(source, byte_offset, 0) == Some('<')
        && char_at(source, byte_offset, 1) == Some('!')
        && char_at(source, byte_offset, 2) == Some('-')
        && char_at(source, byte_offset, 3) == Some('-')
}

pub(super) fn starts_html_close_comment(source: &str, byte_offset: usize) -> bool {
    char_at(source, byte_offset, 0) == Some('-')
        && char_at(source, byte_offset, 1) == Some('-')
        && char_at(source, byte_offset, 2) == Some('>')
}

fn line_prefix_before(source: &str, byte_offset: usize) -> &str {
    let before = &source[..byte_offset];
    let mut line_start = 0;
    for (idx, ch) in before.char_indices().rev() {
        if is_line_terminator(ch) {
            line_start = idx + ch.len_utf8();
            break;
        }
    }
    &before[line_start..]
}

fn is_html_close_comment_prefix(prefix: &str) -> bool {
    let mut idx = 0;
    let len = prefix.len();

    let consume_ws = |i: &mut usize| {
        while *i < len {
            let Some(ch) = prefix[*i..].chars().next() else {
                break;
            };
            if is_horizontal_whitespace(ch) {
                *i += ch.len_utf8();
            } else {
                break;
            }
        }
    };

    consume_ws(&mut idx);

    if idx < len && prefix[idx..].starts_with("*/") {
        idx += 2;
    }

    loop {
        consume_ws(&mut idx);
        if idx >= len || !prefix[idx..].starts_with("/*") {
            break;
        }
        let comment_start = idx + 2;
        let Some(rel_end) = prefix[comment_start..].find("*/") else {
            return false;
        };
        let comment_body = &prefix[comment_start..comment_start + rel_end];
        if comment_body.chars().any(is_line_terminator) {
            return false;
        }
        idx = comment_start + rel_end + 2;
    }

    consume_ws(&mut idx);
    idx == len
}

pub(super) fn html_close_comment_allowed(source: &str, byte_offset: usize) -> bool {
    if source[..byte_offset].ends_with("*/") {
        true
    } else {
        is_html_close_comment_prefix(line_prefix_before(source, byte_offset))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn html_close_prefix_accepts_whitespace_and_inline_comments() {
        assert!(is_html_close_comment_prefix(" \t/*a*/  "));
        assert!(is_html_close_comment_prefix("*/ /*a*/  "));
    }

    #[test]
    fn html_close_prefix_rejects_multiline_comment_body() {
        assert!(!is_html_close_comment_prefix("/*a\n*/"));
    }
}
