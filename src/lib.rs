use std::error::Error;
use std::fmt::{Display, Formatter};
use std::panic::Location;

use chrono::{DateTime, Local, Utc};

pub use crate::_meta as meta;
pub use crate::_meta_extend as meta_extend;

//

#[derive(Clone)]
pub struct LogEvent {
    pub time: DateTime<Utc>,
    pub level: Level,
    pub msg: String,
    pub err: Option<String>,
    pub caller_path: String,
    pub caller_line: u32,
    pub meta: Option<LogEventMeta>,
}

impl LogEvent {
    fn new(time: DateTime<Utc>, level: Level, msg: String, err: Option<String>, caller_path: String, caller_line: u32, meta: Option<LogEventMeta>) -> Self {
        Self {
            time,
            level,
            msg,
            err,
            caller_path,
            caller_line,
            meta,
        }
    }
}

pub trait LogEventHandler {
    fn handle(&self, event: &LogEvent);
}

//

#[derive(Clone)]
pub struct LogEventMeta {
    pub values: Vec<(String, String)>,
}

impl LogEventMeta {
    pub fn new(values: Vec<(String, String)>) -> Self {
        Self {
            values
        }
    }

    pub fn push(&mut self, k: String, v: String) -> &mut Self {
        self.values.push((k, v));
        self
    }

    pub fn concat(&mut self, other: &[(String, String)]) -> &mut Self {
        for (k, v) in other {
            self.values.push((k.clone(), v.clone()));
        }
        self
    }

    fn format(&self) -> String {
        self.values.iter().map(|(k, v)| format!("[{}: {}]", k, v)).collect::<Vec<String>>().join(" ")
    }
}

//

pub struct Logger {
    event_handlers: Vec<Box<dyn LogEventHandler + Send + Sync>>,
}

impl Default for Logger {
    fn default() -> Self {
        Self {
            event_handlers: Vec::new(),
        }
    }
}

impl Logger {
    pub fn new_with_console() -> Self {
        Self {
            event_handlers: vec![Box::new(LogEventConsolePrinter::default())]
        }
    }

    pub fn register<T>(&mut self, handler: T) -> &mut Self where T: 'static + LogEventHandler + Send + Sync {
        self.event_handlers.push(Box::new(handler));
        self
    }

    fn log<T>(&self, level: Level, loc: &Location, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> {
        let event = LogEvent::new(Utc::now(), level, msg.as_ref().to_owned(), None, loc.file().to_owned(), loc.line(), meta);
        for handler in &self.event_handlers {
            handler.handle(&event);
        }
    }

    fn log_with_err<'err, T, TE>(&self, level: Level, loc: &Location, msg: T, err: TE, meta: Option<LogEventMeta>) where T: AsRef<str>, TE: 'err + Error + Sized {
        let event = LogEvent::new(Utc::now(), level, msg.as_ref().to_owned(), Some(format!("{}", err)), loc.file().to_owned(), loc.line(), meta);
        for handler in &self.event_handlers {
            handler.handle(&event);
        }
    }

    #[track_caller]
    pub fn trace<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Trace, Location::caller(), msg, meta); }
    #[track_caller]
    pub fn debug<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Debug, Location::caller(), msg, meta); }
    #[track_caller]
    pub fn stat<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Stat, Location::caller(), msg, meta); }
    #[track_caller]
    pub fn info<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Info, Location::caller(), msg, meta); }
    #[track_caller]
    pub fn notice<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Notice, Location::caller(), msg, meta); }
    #[track_caller]
    pub fn warn<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Warn, Location::caller(), msg, meta); }
    #[track_caller]
    pub fn error<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Error, Location::caller(), msg, meta); }
    #[track_caller]
    pub fn fatal<T>(&self, msg: T, meta: Option<LogEventMeta>) where T: AsRef<str> { self.log(Level::Fatal, Location::caller(), msg, meta); }

    #[track_caller]
    pub fn warn_with_err<'err, T, TE>(&self, msg: T, err: TE, meta: Option<LogEventMeta>) where T: AsRef<str>, TE: 'err + Error + Sized { self.log_with_err(Level::Warn, Location::caller(), msg, err, meta); }
    #[track_caller]
    pub fn error_with_err<'err, T, TE>(&self, msg: T, err: TE, meta: Option<LogEventMeta>) where T: AsRef<str>, TE: 'err + Error + Sized { self.log_with_err(Level::Error, Location::caller(), msg, err, meta); }
    #[track_caller]
    pub fn fatal_with_err<'err, T, TE>(&self, msg: T, err: TE, meta: Option<LogEventMeta>) where T: AsRef<str>, TE: 'err + Error + Sized { self.log_with_err(Level::Fatal, Location::caller(), msg, err, meta); }

    //

    #[track_caller]
    pub fn log_point() {
        let caller = Location::caller();

        #[cfg(debug_assertions)]
        println!(
            "[{}] <> {}:{}",
            Local::now().format("%H:%M:%S"),
            caller.file(),
            caller.line()
        );
    }
}

//

#[macro_export]
macro_rules! _meta {
    ($( $key: expr => $val: expr ),+ $(,)?) => {{
        let mut v: ::std::vec::Vec<(String, String)> = ::std::vec::Vec::new();
        $(
            v.push(($key.to_string(), $val.to_string()));
        )+
        Some(::evlog::LogEventMeta::new(v))
    }}
}

#[macro_export]
macro_rules! _meta_extend {
    ($base: ident, {$( $key: expr => $val: expr ),+ $(,)?}) => {{
        let mut v = $base.unwrap();
        $(
            v.push($key.to_string(), $val.to_string());
        )+
        Some(v)
    }}
}

//

#[derive(Clone)]
pub enum Level {
    Trace,
    Debug,
    Stat,
    Info,
    Notice,
    Warn,
    Error,
    Fatal,
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Level::Trace => "TRCE",
            Level::Debug => "DEBG",
            Level::Stat => "STAT",
            Level::Info => "INFO",
            Level::Notice => "NOTE",
            Level::Warn => "WARN",
            Level::Error => "ERRR",
            Level::Fatal => "FATL",
        })
    }
}

//

const PREFIX_PROJECT: &str = "/project/";
const PREFIX_CRATES: &str = "crates/";

pub struct LogEventConsolePrinter {}

impl Default for LogEventConsolePrinter {
    fn default() -> Self { Self {} }
}

impl LogEventConsolePrinter {
    fn fmt_prefix(time: &DateTime<Utc>, level: &Level, caller_path: &str, caller_line: u32) -> String {
        let mut caller_path = caller_path.to_owned();

        if caller_path.starts_with(PREFIX_PROJECT) {
            caller_path = caller_path.chars().skip(PREFIX_PROJECT.len()).take(caller_path.len() - PREFIX_PROJECT.len()).collect();
        }

        if caller_path.starts_with(PREFIX_CRATES) {
            caller_path = caller_path.chars().skip(PREFIX_CRATES.len()).take(caller_path.len() - PREFIX_CRATES.len()).collect();
        }

        format!(
            "[{}] [{}] [{}:{}]",
            time.format("%H:%M:%S"),
            level,
            caller_path, caller_line,
        )
    }

    fn fmt_suffix(meta: &LogEventMeta) -> String {
        meta.format()
    }
}

impl LogEventHandler for LogEventConsolePrinter {
    fn handle(&self, event: &LogEvent) {
        let text = match &event.err {
            None => event.msg.to_owned(),
            Some(e) => format!("{} | Error: {}", event.msg, e)
        };

        match event.meta {
            None =>
                println!("{} {}", LogEventConsolePrinter::fmt_prefix(&event.time, &event.level, &event.caller_path, event.caller_line), text),
            Some(ref v) =>
                println!("{} {} {}", LogEventConsolePrinter::fmt_prefix(&event.time, &event.level, &event.caller_path, event.caller_line), text, LogEventConsolePrinter::fmt_suffix(v)),
        }
    }
}
