use std::collections::HashMap;

use crate::{ast::*, errors::*, llir};

mod rename;
mod type_check;
mod lower;

/// Type check, lower, and compile an Ast. Return the LLIR along with a map from qualified names to unique names.
pub fn transform(ast: &mut Ast) -> Res<(Vec<llir::LLIR>, HashMap<String, String>)> {
	//rename::rename(ast)?;
	type_check::type_check(ast, &mut HashMap::new())?;
	//ast.print(0);
	let (llir, map) = lower::lower_ast(ast);
	Ok((llir, map))
}
