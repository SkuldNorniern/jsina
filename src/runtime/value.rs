#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Int(i32),
    Number(f64),
    String(String),
}

impl Value {
    pub fn to_i64(&self) -> i64 {
        match self {
            Value::Int(n) => *n as i64,
            Value::Number(n) => *n as i64,
            _ => 0,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Undefined => write!(f, "undefined"),
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_equality() {
        assert_eq!(Value::Undefined, Value::Undefined);
        assert_eq!(Value::Int(42), Value::Int(42));
        assert_ne!(Value::Int(1), Value::Int(2));
    }
}
