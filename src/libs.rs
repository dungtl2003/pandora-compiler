use crate::interpreter::eval::Value;

pub mod math;
pub mod std;

pub trait Library {
    fn get_function(
        &self,
        name: &str,
    ) -> Option<&Box<dyn Fn(Vec<(Value, bool)>) -> Result<Value, String>>>;
}
