use super::codes::ErrorCode;
use super::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub code: String,
    pub severity: Severity,
    pub message: String,
    pub primary_span: Option<Span>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(
        code: impl std::fmt::Display,
        message: impl Into<String>,
        primary_span: Option<Span>,
    ) -> Self {
        Self {
            code: code.to_string(),
            severity: Severity::Error,
            message: message.into(),
            primary_span,
            notes: Vec::new(),
        }
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn format(&self, source: Option<&str>) -> String {
        let mut out = String::new();

        let loc = self
            .primary_span
            .map(|s| format!(" at {}", s))
            .unwrap_or_default();
        out.push_str(&format!(
            "{}: {} ({}){}\n",
            self.severity_label(),
            self.message,
            self.code,
            loc
        ));

        if let (Some(span), Some(src)) = (self.primary_span, source)
            && let Some(snippet) = self.extract_snippet(src, span)
        {
            out.push_str(&snippet);
        }

        for note in &self.notes {
            out.push_str(&format!("  note: {}\n", note));
        }

        out
    }

    fn severity_label(&self) -> &'static str {
        match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
        }
    }

    fn extract_snippet(&self, source: &str, span: Span) -> Option<String> {
        let lines: Vec<&str> = source.lines().collect();
        if span.start.line == 0 || span.start.line > lines.len() {
            return None;
        }

        let line_idx = span.start.line - 1;
        let line = lines.get(line_idx)?;
        let mut out = format!("  {} | {}\n", span.start.line, line);

        let col = span.start.column.saturating_sub(1);
        let end_col = span.end.column.min(line.len() + 1);
        let underline_len = end_col.saturating_sub(col).max(1);
        let pad = " ".repeat(span.start.line.to_string().len());
        out.push_str(&format!(
            "  {} | {}{}\n",
            pad,
            " ".repeat(col),
            "^".repeat(underline_len)
        ));

        Some(out)
    }
}

/// Build a diagnostic for "callee is not a function" runtime errors.
/// Message should be the full TypeError string (e.g. "TypeError: callee is not a function (got number)").
pub fn callee_not_function_diagnostic(message: impl Into<String>) -> Diagnostic {
    let msg = message.into();
    let note = if let Some(got) = msg.split("(got ").nth(1).and_then(|s| s.strip_suffix(')')) {
        format!("received type: {}", got.trim())
    } else {
        "the value being called must be a function, builtin, or method".to_string()
    };
    Diagnostic::error(ErrorCode::RunCalleeNotFunction, msg, None).with_note(note)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_error_creation() {
        let d = Diagnostic::error("JSINA-001", "test error", None);
        assert_eq!(d.code, "JSINA-001");
        assert!(matches!(d.severity, Severity::Error));
    }

    #[test]
    fn diagnostic_with_note() {
        let d = Diagnostic::error("JSINA-002", "msg", None).with_note("hint");
        assert_eq!(d.notes.len(), 1);
        assert_eq!(d.notes[0], "hint");
    }

    #[test]
    fn format_without_source() {
        let d = Diagnostic::error("JSINA-003", "parse failed", None);
        let s = d.format(None);
        assert!(s.contains("parse failed"));
        assert!(s.contains("JSINA-003"));
    }

    #[test]
    fn callee_not_function_diagnostic_has_code_and_note() {
        use super::super::codes::ErrorCode;
        let d = super::callee_not_function_diagnostic(
            "TypeError: callee is not a function (got number)",
        );
        assert_eq!(d.code, ErrorCode::RunCalleeNotFunction.as_str());
        assert_eq!(
            d.message,
            "TypeError: callee is not a function (got number)"
        );
        assert_eq!(d.notes.len(), 1);
        assert_eq!(d.notes[0], "received type: number");
    }
}
