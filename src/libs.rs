use crate::{
    interpreter::eval::{Value, ValueKind},
    span_encoding::Span,
};

pub mod math;
pub mod std;

pub trait Library {
    fn get_function(
        &self,
        name: &str,
    ) -> Option<&Box<dyn Fn(CallerAttrs, Vec<(Value, bool)>) -> Result<ValueKind, String>>>;
}

pub struct CallerAttrs {
    pub span: Span,
    pub prefix_span: Span,
}
