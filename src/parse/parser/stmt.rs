// block_statement = { statement* }
// statement = block_statement
//             | declaration_statement
//             | expression_statement
//             | if_statement
//
// expression_statement = expression ';'
// expression = assignment_expression
//              | binary_expression
//              | unary_expression
//              | literal
//
// declaration_statement = declaration
// declaration = variable_declaration
//               | function_declaration
//               | class_declaration
//               | enum_declaration
//               | interface_declaration
//
// if_statement = 'if' expression block_statement ('elif' expression block_statement)* 'else'
//                block_statement
//
// variable_declaration = 'var' 'mut'? identifier: type_specifier ('=' expression)? ';'
// type_specifier = primitive_type | ...
// primitive_type = 'int' | 'float' | 'bool' | 'char'
