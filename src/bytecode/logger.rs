//! Logging abstraction for CFG analysis and structuring

/// Logging levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

/// Generic logger trait for CFG analysis and structuring
pub trait Logger {
    /// Log a message at the specified level
    fn log(&self, level: LogLevel, message: &str);

    /// Log a debug message
    fn debug(&self, message: &str) {
        self.log(LogLevel::Debug, message);
    }

    /// Log an info message
    fn info(&self, message: &str) {
        self.log(LogLevel::Info, message);
    }

    /// Log a warning message
    fn warn(&self, message: &str) {
        self.log(LogLevel::Warn, message);
    }

    /// Log an error message
    fn error(&self, message: &str) {
        self.log(LogLevel::Error, message);
    }
}

/// Logger that writes to stderr
pub struct StderrLogger {
    min_level: LogLevel,
}

impl StderrLogger {
    pub fn new(min_level: LogLevel) -> Self {
        Self { min_level }
    }

    pub fn all() -> Self {
        Self::new(LogLevel::Debug)
    }

    pub fn info_and_above() -> Self {
        Self::new(LogLevel::Info)
    }

    pub fn warn_and_above() -> Self {
        Self::new(LogLevel::Warn)
    }

    pub fn error_only() -> Self {
        Self::new(LogLevel::Error)
    }
}

impl Logger for StderrLogger {
    fn log(&self, level: LogLevel, message: &str) {
        if level >= self.min_level {
            eprintln!("{}", message);
        }
    }
}

/// Logger that discards all messages (silent)
pub struct NullLogger;

impl Logger for NullLogger {
    fn log(&self, _level: LogLevel, _message: &str) {
        // Do nothing
    }
}

/// Logger that collects messages in memory
pub struct BufferedLogger {
    messages: std::sync::Mutex<Vec<(LogLevel, String)>>,
    min_level: LogLevel,
}

impl BufferedLogger {
    pub fn new(min_level: LogLevel) -> Self {
        Self {
            messages: std::sync::Mutex::new(Vec::new()),
            min_level,
        }
    }

    pub fn all() -> Self {
        Self::new(LogLevel::Debug)
    }

    /// Get all logged messages
    pub fn messages(&self) -> Vec<(LogLevel, String)> {
        self.messages.lock().unwrap().clone()
    }

    /// Clear all messages
    pub fn clear(&self) {
        self.messages.lock().unwrap().clear();
    }
}

impl Logger for BufferedLogger {
    fn log(&self, level: LogLevel, message: &str) {
        if level >= self.min_level {
            self.messages
                .lock()
                .unwrap()
                .push((level, message.to_string()));
        }
    }
}

/// Helper to create a reference to a logger that can be passed around
pub type LoggerRef<'a> = &'a dyn Logger;
