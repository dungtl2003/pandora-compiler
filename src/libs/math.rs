use std::collections::HashMap;

use crate::interpreter::eval::{Value, ValueKind};

use super::{CallerAttrs, Library};

pub struct MathLib {
    pub functions:
        HashMap<String, Box<dyn Fn(CallerAttrs, Vec<(Value, bool)>) -> Result<ValueKind, String>>>,
}

impl Library for MathLib {
    fn get_function(
        &self,
        name: &str,
    ) -> Option<&Box<dyn Fn(CallerAttrs, Vec<(Value, bool)>) -> Result<ValueKind, String>>> {
        self.functions.get(name)
    }
}

impl MathLib {
    pub fn new() -> Self {
        let mut lib = MathLib {
            functions: HashMap::new(),
        };

        lib.register_functions();
        lib
    }

    fn register_functions(&mut self) {
        self.register_sqrt_function();
        self.register_pow_function();
        self.register_abs_function();
        self.register_gcd_function();

        self.register_ceil_function();
        self.register_floor_function();
        self.register_round_function();

        self.register_sin_function();
        self.register_cos_function();
        self.register_tan_function();

        self.register_log_function();
        self.register_ln_function();
    }

    fn register_ln_function(&mut self) {
        // ln() function
        self.functions.insert(
            "ln".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("ln() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Float(f.ln())),
                    _ => Err("ln() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_log_function(&mut self) {
        // log() function
        self.functions.insert(
            "log".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 2 {
                    return Err("log() takes exactly 2 argument".to_string());
                }

                match (&args[0].0.kind, &args[1].0.kind) {
                    (ValueKind::Float(i), ValueKind::Float(j)) => {
                        Ok(ValueKind::Float(i.log(j.clone())))
                    }
                    _ => Err("log() takes two floats".to_string()),
                }
            }),
        );
    }

    fn register_tan_function(&mut self) {
        // tan() function
        self.functions.insert(
            "tan".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("tan() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Float(f.to_radians().tan())),
                    _ => Err("tan() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_cos_function(&mut self) {
        // cos() function
        self.functions.insert(
            "cos".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("cos() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Float(f.to_radians().cos())),
                    _ => Err("cos() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_sin_function(&mut self) {
        // sin() function
        self.functions.insert(
            "sin".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("sin() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Float(f.to_radians().sin())),
                    _ => Err("sin() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_floor_function(&mut self) {
        // floor() function
        self.functions.insert(
            "floor".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("floor() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Int(f.floor() as i64)),
                    _ => Err("floor() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_ceil_function(&mut self) {
        // ceil() function
        self.functions.insert(
            "ceil".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("ceil() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Int(f.ceil() as i64)),
                    _ => Err("ceil() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_gcd_function(&mut self) {
        // gcd() function
        self.functions.insert(
            "gcd".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 2 {
                    return Err("gcd() takes exactly 2 arguments".to_string());
                }

                fn calculate_gcd(mut a: i64, mut b: i64) -> i64 {
                    a = a.abs();
                    b = b.abs();
                    while b != 0 {
                        let temp = b;
                        b = a % b;
                        a = temp;
                    }
                    a
                }

                match (&args[0].0.kind, &args[1].0.kind) {
                    (ValueKind::Int(i), ValueKind::Int(j)) => {
                        Ok(ValueKind::Int(calculate_gcd(*i, *j)))
                    }
                    _ => Err("gcd() takes two integers".to_string()),
                }
            }),
        );
    }

    fn register_abs_function(&mut self) {
        // abs() function
        self.functions.insert(
            "abs".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("abs() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Float(f.abs())),
                    _ => Err("abs() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_round_function(&mut self) {
        // round() function
        self.functions.insert(
            "round".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("round() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Int(f.round() as i64)),
                    _ => Err("round() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_sqrt_function(&mut self) {
        // sqrt() function
        self.functions.insert(
            "sqrt".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 1 {
                    return Err("sqrt() takes exactly 1 argument".to_string());
                }
                match &args[0].0.kind {
                    ValueKind::Float(f) => Ok(ValueKind::Float(f.sqrt())),
                    _ => Err("sqrt() takes a float".to_string()),
                }
            }),
        );
    }

    fn register_pow_function(&mut self) {
        // pow() function
        self.functions.insert(
            "pow".to_string(),
            Box::new(|cattrs, args| {
                if args.len() != 2 {
                    return Err("pow() takes exactly 2 arguments".to_string());
                }
                match (&args[0].0.kind, &args[1].0.kind) {
                    (ValueKind::Float(i), ValueKind::Float(j)) => {
                        Ok(ValueKind::Float((i.powf(*j)) as f64))
                    }
                    _ => Err("pow() takes two floats".to_string()),
                }
            }),
        );
    }
}
