use std::collections::HashMap;

use crate::interpreter::{
    errors::IError,
    eval::{Value, ValueKind},
};

use super::{CallerAttrs, Library};
use ::std::io::{self, Write};

pub struct StdLib {
    pub functions: HashMap<
        String,
        Box<dyn Fn(CallerAttrs, Vec<(Value, bool)>) -> Result<ValueKind, Vec<IError>>>,
    >,
}

impl Library for StdLib {
    fn get_function(
        &self,
        name: &str,
    ) -> Option<&Box<dyn Fn(CallerAttrs, Vec<(Value, bool)>) -> Result<ValueKind, Vec<IError>>>>
    {
        self.functions.get(name)
    }
}

impl StdLib {
    pub fn new() -> Self {
        let mut lib = StdLib {
            functions: HashMap::new(),
        };

        lib.register_functions();
        lib
    }

    fn register_functions(&mut self) {
        self.register_input_function();
        self.register_print_function();
        self.register_println_function();

        self.register_lower_function();
        self.register_upper_function();

        self.register_str_len_function();
        self.register_array_len_function();

        self.register_delay_function();
    }

    fn register_delay_function(&mut self) {
        // delay() function
        self.functions.insert(
            "delay".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "delay() takes exactly 1 argument".to_string(),
                    }]);
                }
                match &args[0].0.kind {
                    ValueKind::Int(ms) => {
                        std::thread::sleep(std::time::Duration::from_millis(*ms as u64));
                    }
                    _ => {
                        return Err(vec![IError::PredefinedError {
                            span: cattrs.prefix_span,
                            message: "delay() takes an integer".to_string(),
                        }]);
                    }
                }
                Ok(ValueKind::Unit)
            }),
        );
    }

    fn register_str_len_function(&mut self) {
        // strlen() function for strings
        self.functions.insert(
            "strlen".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "strlen() takes exactly 1 argument".to_string(),
                    }]);
                }
                match &args[0].0.kind {
                    ValueKind::Str(s) => Ok(ValueKind::Int(s.len() as i64)),
                    _ => Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "strlen() takes a string".to_string(),
                    }]),
                }
            }),
        );
    }

    fn register_array_len_function(&mut self) {
        // arrlen() function for arrays
        self.functions.insert(
            "arrlen".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "arrlen() takes exactly 1 argument".to_string(),
                    }]);
                }
                match &args[0].0.kind {
                    ValueKind::Array(arr) => Ok(ValueKind::Int(arr.len() as i64)),
                    _ => Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "arrlen() takes an array".to_string(),
                    }]),
                }
            }),
        );
    }

    fn register_lower_function(&mut self) {
        // lower() function
        self.functions.insert(
            "lower".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "lower() takes exactly 1 argument".to_string(),
                    }]);
                }
                match &args[0].0.kind {
                    ValueKind::Str(s) => Ok(ValueKind::Str(s.to_lowercase())),
                    _ => Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "lower() takes a string".to_string(),
                    }]),
                }
            }),
        );
    }

    fn register_upper_function(&mut self) {
        // upper() function
        self.functions.insert(
            "upper".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "upper() takes exactly 1 argument".to_string(),
                    }]);
                }
                match &args[0].0.kind {
                    ValueKind::Str(s) => Ok(ValueKind::Str(s.to_uppercase())),
                    _ => Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "upper() takes a string".to_string(),
                    }]),
                }
            }),
        );
    }

    fn register_input_function(&mut self) {
        // input() function
        self.functions.insert(
            "input".to_string(),
            Box::new(|cattrs, args| {
                if !args.is_empty() {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "input() takes no arguments".to_string(),
                    }]);
                }
                let mut input = String::new();
                std::io::stdin()
                    .read_line(&mut input)
                    .expect("Failed to read line");
                Ok(ValueKind::Str(input.trim().to_string()))
            }),
        );
    }

    fn register_println_function(&mut self) {
        // println() function
        self.functions.insert(
            "println".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "println() takes exactly 1 argument".to_string(),
                    }]);
                }
                match &args[0].0.kind {
                    ValueKind::Str(s) => {
                        println!("{}", s);
                    }
                    _ => {
                        return Err(vec![IError::PredefinedError {
                            span: cattrs.prefix_span,
                            message: "println() takes a string".to_string(),
                        }]);
                    }
                }
                Ok(ValueKind::Unit)
            }),
        );
    }

    fn register_print_function(&mut self) {
        // print() function
        self.functions.insert(
            "print".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err(vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: "print() takes exactly 1 argument".to_string(),
                    }]);
                }
                match &args[0].0.kind {
                    ValueKind::Str(s) => {
                        print!("{}", s);
                    }
                    _ => {
                        return Err(vec![IError::PredefinedError {
                            span: cattrs.prefix_span,
                            message: "print() takes a string".to_string(),
                        }]);
                    }
                }
                io::stdout().flush().map_err(|e| {
                    vec![IError::PredefinedError {
                        span: cattrs.prefix_span,
                        message: format!("{e}"),
                    }]
                })?;
                Ok(ValueKind::Unit)
            }),
        );
    }
}
