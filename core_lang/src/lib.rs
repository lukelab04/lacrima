use vm::Vm;

use crate::ast::Ast;
use crate::errors::Res;

#[macro_use]
pub mod ast;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod llir;
pub mod transformations;
pub mod vm;

/// Compile an Ast and start a VM.
/// # Errors
/// Returns an error for any renaming, typechecking, or Ast lowering issues.
pub fn compile(ast: &mut Ast) -> Res<Vm> {
	let (mut llir, map) = transformations::transform(ast)?;
	let (bc, map) = llir::to_bytecode(&mut llir, map);
	Ok(Vm::new(bc, map))
}

