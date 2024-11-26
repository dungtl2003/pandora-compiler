use crate::ast::{
    AngleBracketedArg, AngleBracketedArgs, GenericArg, GenericArgs, Ident, Path, PathSegment,
};

use super::IrRustGenerator;

impl<'ctx> IrRustGenerator<'ctx> {
    pub fn generate_path(&self, path: &Box<Path>) -> String {
        let Path { segments, .. } = path.as_ref();
        let segments = segments.iter().map(|s| self.generate_path_segment(s));
        segments.collect::<Vec<String>>().join("::")
    }

    fn generate_path_segment(&self, segment: &PathSegment) -> String {
        let PathSegment { ident, args } = segment;
        let Ident { name, .. } = ident;

        match args {
            Some(args) => {
                let args = self.generate_path_args(args);
                format!("{}::{}", name, args)
            }
            None => name.as_str().to_string(),
        }
    }

    fn generate_path_args(&self, args: &Box<GenericArgs>) -> String {
        match args.as_ref() {
            GenericArgs::AngleBracketed(args) => self.generate_path_angle_bracketed_args(args),
        }
    }

    fn generate_path_angle_bracketed_args(&self, args: &AngleBracketedArgs) -> String {
        let AngleBracketedArgs { args, span: _ } = args;
        let args = args
            .iter()
            .map(|arg| self.generate_path_angle_bracked_arg(arg))
            .collect::<Vec<String>>()
            .join(", ")
            .to_string();

        format!("<{}>", args)
    }

    fn generate_path_angle_bracked_arg(&self, arg: &AngleBracketedArg) -> String {
        match arg {
            AngleBracketedArg::Arg(arg) => self.generate_path_generic_arg(arg),
        }
    }

    fn generate_path_generic_arg(&self, arg: &GenericArg) -> String {
        match arg {
            GenericArg::Type(ty) => self.generate_ty(ty),
        }
    }
}
