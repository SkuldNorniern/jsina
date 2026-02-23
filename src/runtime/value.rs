#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Int(i32),
    Number(f64),
    String(String),
    Object(usize),
    Array(usize),
}

impl Value {
    pub fn to_i64(&self) -> i64 {
        match self {
            Value::Int(n) => *n as i64,
            Value::Number(n) => *n as i64,
            _ => 0,
        }
    }

    pub fn is_object(&self) -> bool {
        matches!(self, Value::Object(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    pub fn as_object_id(&self) -> Option<usize> {
        match self {
            Value::Object(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_array_id(&self) -> Option<usize> {
        match self {
            Value::Array(id) => Some(*id),
            _ => None,
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
            Value::Object(_) => write!(f, "[object Object]"),
            Value::Array(_) => write!(f, "[object Array]"),
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
