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
    pub fn error(code: impl Into<String>, message: impl Into<String>, primary_span: Option<Span>) -> Self {
        Self {
            code: code.into(),
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
        out.push_str(&format!("{}: {} ({}){}\n", self.severity_label(), self.message, self.code, loc));

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
        out.push_str(&format!("  {} | {}{}\n", pad, " ".repeat(col), "^".repeat(underline_len)));

        Some(out)
    }
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
}
