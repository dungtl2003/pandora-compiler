use std::sync::Arc;

use miette::NamedSource;
use crate::error_handler::ErrorHandler;

#[derive(Debug)]
pub struct SessionGlobal {
    pub file: Arc<SourceFile>,
    pub has_errors: bool,
    pub error_handler: ErrorHandler,
}

pub type BytePos = u32;

pub type SourceFile = NamedSource<Arc<String>>;

impl SessionGlobal {
    pub fn new(file:Arc<SourceFile>) -> Self {
        // let file_path = "src/trash/".to_string();
        // let file_name =
        //     //"program.box"
        //     //"var_decl.box"
        //     //"path.box"
        //     //"expr.box"
        //     //"test.box"
        //     //"main.box"
        //     //"unterminated_block_comment.box"
        //     //"number_literal_error.box"
        //     //"unterminated_raw_str.box"
        //     //"too_many_hashes_raw_str.box"
        //     //"unterminated_char.box"
        //     //"unescape_error.box"
        //     //"if_stmt.box"
        //     "fun_item.box"
        //     //"class_item.box"
        // ;
        // let data = Arc::new(
        //     fs::read_to_string(file_path.clone() + file_name).expect("unable to read file"),
        // );
        // let source = NamedSource::new(file_name, Arc::clone(&data));
        Self {
            file: Arc::clone(&file),
            has_errors: false,
            error_handler:ErrorHandler::new(Arc::clone(&file)),
        }
    }
}
