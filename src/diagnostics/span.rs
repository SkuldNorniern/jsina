/// Source position and span tracking for error reporting and debugging
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub byte_offset: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, byte_offset: usize) -> Self {
        Self {
            line,
            column,
            byte_offset,
        }
    }

    pub fn start() -> Self {
        Self::new(1, 1, 0)
    }

    pub fn advance(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.byte_offset += ch.len_utf8();
    }

    pub fn advance_by(&mut self, text: &str) {
        for ch in text.chars() {
            self.advance(ch);
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn point(start: Position) -> Self {
        Self::new(start, start)
    }

    pub fn from_text(start: Position, text: &str) -> Self {
        let mut end = start;
        end.advance_by(text);
        Self::new(start, end)
    }

    pub fn length(&self) -> usize {
        self.end.byte_offset.saturating_sub(self.start.byte_offset)
    }

    pub fn is_point(&self) -> bool {
        self.start == self.end
    }

    pub fn is_single_line(&self) -> bool {
        self.start.line == self.end.line
    }

    pub fn contains_byte_offset(&self, byte_offset: usize) -> bool {
        self.start.byte_offset <= byte_offset && byte_offset <= self.end.byte_offset
    }

    pub fn merge(self, other: Span) -> Span {
        let start = if self.start.byte_offset <= other.start.byte_offset {
            self.start
        } else {
            other.start
        };
        let end = if self.end.byte_offset >= other.end.byte_offset {
            self.end
        } else {
            other.end
        };
        Span::new(start, end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_advance() {
        let mut p = Position::start();
        assert_eq!(p.line, 1);
        assert_eq!(p.column, 1);
        p.advance('a');
        assert_eq!(p.column, 2);
        p.advance('\n');
        assert_eq!(p.line, 2);
        assert_eq!(p.column, 1);
    }

    #[test]
    fn span_from_text() {
        let start = Position::start();
        let span = Span::from_text(start, "foo");
        assert_eq!(span.length(), 3);
    }

    #[test]
    fn span_flags_and_contains() {
        let start = Position::new(1, 1, 0);
        let end = Position::new(1, 4, 3);
        let span = Span::new(start, end);
        assert!(span.is_single_line());
        assert!(!span.is_point());
        assert!(span.contains_byte_offset(1));
        assert!(!span.contains_byte_offset(5));
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start.line == self.end.line {
            if self.start.column == self.end.column {
                write!(f, "{}:{}", self.start.line, self.start.column)
            } else {
                write!(
                    f,
                    "{}:{}-{}",
                    self.start.line, self.start.column, self.end.column
                )
            }
        } else {
            write!(
                f,
                "{}:{}-{}:{}",
                self.start.line, self.start.column, self.end.line, self.end.column
            )
        }
    }
}
