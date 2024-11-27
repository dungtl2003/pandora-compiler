use std::{
    fs::{File, OpenOptions},
    io::Write,
    path::Path,
};

use crate::{
    ast::{Ast, Ident, Stmt},
    sem::{
        scope::{Environment, Wrapper},
        variable::Variable,
    },
};

mod expr;
mod path;
mod stmt;
mod ty;

pub struct IrRustGenerator<'ctx> {
    builder: Builder,
    context: &'ctx Environment,
}

impl<'ctx> IrRustGenerator<'ctx> {
    pub fn new(context: &'ctx Environment) -> Self {
        Self {
            builder: Builder::new(),
            context,
        }
    }

    pub fn generate_code(&mut self, ast: &Ast) {
        self.generate_stmts(&ast.stmts);
        self.builder.print_to_file();
    }

    fn generate_stmts(&mut self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts {
            self.builder.push_str(&self.generate_stmt(stmt));
        }
    }

    fn resolve_ident(&self, ident: &Ident) -> Wrapper<Variable> {
        let Ident {
            name,
            span,
            scope_id,
        } = ident;

        let scope_id = scope_id.as_ref().unwrap().as_str();
        let name = name.clone();
        let span = span.clone();
        self.context.lookup_variable(name, span, scope_id).unwrap()
    }
}

struct Builder {
    code: String,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            code: String::new(),
        }
    }

    pub fn push_str(&mut self, s: &str) {
        self.code.push_str(s);
    }

    pub fn print_to_file(&self) {
        // Write to file
        let path = Path::new("output.rs");

        let mut file: File = OpenOptions::new()
            .create(true) // Create the file if it doesn't exist
            .write(true) // Enable write access
            .truncate(true) // Truncate the file to zero length if it exists
            .open(path)
            .expect("Unable to open file");

        file.write_all(self.code.as_bytes())
            .expect("Unable to write to file");
    }
}
