#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Int(i32),
    Number(f64),
    String(String),
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
