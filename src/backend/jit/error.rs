use lamina::LaminaError;

#[derive(Debug)]
pub enum BackendError {
    Io(std::io::Error),
    Lamina(LaminaError),
    Parse(String),
    Process(String),
}

impl std::fmt::Display for BackendError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BackendError::Io(e) => write!(f, "I/O error: {}", e),
            BackendError::Lamina(e) => write!(f, "Lamina error: {}", e),
            BackendError::Parse(msg) => write!(f, "Parse error: {}", msg),
            BackendError::Process(msg) => write!(f, "Process error: {}", msg),
        }
    }
}

impl std::error::Error for BackendError {}

impl From<std::io::Error> for BackendError {
    fn from(err: std::io::Error) -> Self {
        BackendError::Io(err)
    }
}

impl From<LaminaError> for BackendError {
    fn from(err: LaminaError) -> Self {
        BackendError::Lamina(err)
    }
}
