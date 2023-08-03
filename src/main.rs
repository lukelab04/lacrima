use std::path::Path;
use core_lang::errors::*;
use core_lang::vm::values::Value;
use crate::parser::scopes::ScopeManager;

mod ast;
mod lexer;
mod parser;
mod transform;
mod namespace;

fn main() {
	match run() {
		Ok(_) => (),
		Err(e) => panic!("{}", e.to_string()),
	}
}

fn run() -> Res<()> {
	// Build the project structure & namespaces
	let structure = namespace::build_project_structure(Path::new("./lc code/main.lac"))?;
	let namespaces = structure.namespaces;
	let resolver = structure.resolver;

	// Construct an Ast with all nodes from all files
	let mut all_nodes = vec![];
	for (n, t) in structure.tokens {
		let module = parser::parse(t, ScopeManager::new(n, namespaces.clone(), resolver.clone()))?;
		match module {
			ast::AstNode::Module(mut nodes) => all_nodes.append(&mut nodes),
			_ => panic!("'parse' should have returned Module (compiler error)")
		}
	}
	let mut ast = ast::AstNode::Module(all_nodes);

	// Lower the ast to core
	let mut lowered = transform::to_lower_ast(&mut ast);
	//lowered.print(0);

	// Compile core to bytecode
	let mut vm = match core_lang::compile(&mut lowered) {
		Ok(bc) => bc,
		Err(e) => panic!("{}", e.to_string()),
	};

	// Register necessary functions and run
	vm.register_fn("print", |vals| {
		println!("{:?}", vals[0]);
		Value::None
	});
	vm.call("root::main::main", vec![Value::Num(0.0)]);

	Ok(())
}
