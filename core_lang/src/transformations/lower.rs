use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::atomic::AtomicUsize;

use crate::llir::*;
use crate::ast::*;
use crate::vm::values::Value;


/// A multithread-safe way to generate unique labels.
fn gen_unique_label() -> String {
	static COUNT: AtomicUsize = AtomicUsize::new(0);
	format!("##__LABEL{}", COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
}

/// Given an Ast, return the LLIR and a map from qualified to unique names
pub fn lower_ast(ast: &mut Ast) -> (Vec<LLIR>, HashMap<String, String>) {
	let mut lambdas = HashMap::new();
	let mut qualified_to_unique = HashMap::new();

	// We must lift all lambdas to the top level. We replace them will calls to the new top level functions.
	lambda_lift(ast, &mut lambdas, &mut CapturedVarManager::new());

	let mut lambdas_cloned = lambdas.clone();
	let mut code = vec![];
	// Compile the code that was removed from lifting lambdas.
	for (name, (node, _)) in lambdas.iter_mut() {
		match node {
			Ast::Abst(a) => lower_toplvl_fn(a, name.clone(), &mut code, &mut lambdas_cloned),
			Ast::Const(c) => lower_const(c, name.clone(), &mut code, &mut lambdas_cloned, &mut qualified_to_unique),
			Ast::Constructor(c) => lower_cons(c, &mut code),
			_ => panic!("Only functions and constructors can be top level (this is a compiler bug)"),
		}
	}

	// Compile the rest of the code
	lower(ast, &mut code, &mut lambdas);

	(code, qualified_to_unique)
}

/// Lower a const function into LLIR
fn lower_const(c: &mut Const, name: String, code: &mut Vec<LLIR>, map: &mut HashMap<String, (Ast, Vec<String>)>, qual_to_un: &mut HashMap<String, String>) {
	let abst = match c.rhs.as_mut() {
		Ast::Abst(a) => a,
		_ => panic!("Body of a Const should be Abst (compiler error)")
	};
	let arg = &cast!(abst.ident.as_ref(), Ast::Ident).name;

	// If there is a fully qualified name associated here, attach it.
	if let Some(q) = &c.fully_qualified_name {
		qual_to_un.insert(q.clone(), name.clone());
	}

	// Declare the function, compile, return.
	code.push(LLIR::FnDecl(name, vec![arg.clone()]));
	lower(&mut abst.body, code, map);
	code.push(LLIR::Ret);
}

/// Compile a top level function into LLIR.
fn lower_toplvl_fn(a: &mut Abst, name: String, code: &mut Vec<LLIR>, map: &mut HashMap<String, (Ast, Vec<String>)>) {
	let arg = &cast!(a.ident.as_ref(), Ast::Ident).name;
	code.push(LLIR::FnDecl(name, vec![arg.clone()]));
	// Gen code for function
	lower(&mut a.body, code, map);
	// Return
	code.push(LLIR::Ret);
}

/// Compile a constructor into LLIR.
fn lower_cons(c: &mut Constructor, code: &mut Vec<LLIR>) {
	let id = &cast!(c.id.as_ref(), Ast::Ident).name;
	code.push(LLIR::FnDecl(id.clone(), vec![]));
	// Body of a constructor function
	// Arg is already top of stack, so we can directly make the variant
	code.push(LLIR::MkVariant(c.tag));
	// Return
	code.push(LLIR::Ret);
}

/// Lower an AST into LLIR.
fn lower(ast: &mut Ast, code: &mut Vec<LLIR>, lambdas: &mut HashMap<String, (Ast, Vec<String>)>) {
	match ast {
		// Modules are pretty straightforward, we just compile all the inner nodes.
		Ast::Module(m) => {
			for node in m {
				if let Ast::None = node { continue; }
				lower(node, code, lambdas);
			}
		}
		Ast::Appl(a) => {
			// Put arg onto stack
			lower(&mut a.right, code, lambdas);
			// Put func onto stack
			lower(&mut a.left, code, lambdas);
			// Call with 1 arg
			code.push(LLIR::Call(1));
		}
		Ast::Extern(e) => {
			// Push args back to front
			for item in e.args.iter_mut().rev() {
				lower(item, code, lambdas);
			}
			code.push(LLIR::CallExtern(cast!(e.id.as_ref(), Ast::Ident).name.clone(), e.args.len() as u64));
		}
		Ast::TAppl(t) => lower(&mut t.left, code, lambdas),
		Ast::TAbst(t) => lower(&mut t.body, code, lambdas),
		Ast::Abst(_) => panic!("All absts should have been lifted (this is a compiler error)"),
		Ast::Ident(i) => {
			// This identifier used to be a lambda, and was lifted to global scope.
			// We need to create and bind an environment for it.
			if let Some(_) = lambdas.get(&i.name) {
				code.push(LLIR::MakeEnv(i.name.clone()));
			} else {
				// Load the variable
				code.push(LLIR::Load(i.name.clone()));
			}
		}
		Ast::If(i) => {
			let f_label = gen_unique_label();
			let done_label = gen_unique_label();

			// Evaluate the arg
			lower(&mut i.condition, code, lambdas);
			// If arg is false, jump to false block
			code.push(LLIR::GotoIfFalse(f_label.clone()));
			// Otherwise, continue to true block
			lower(&mut i.t_stmt, code, lambdas);
			// Jump past false block
			code.push(LLIR::Goto(done_label.clone()));
			// Gen false block
			code.push(LLIR::Label(f_label));
			lower(&mut i.f_stmt, code, lambdas);
			// Gen ending
			code.push(LLIR::Label(done_label));
		}
		Ast::Match(m) => {
			// It'll be helpful to have a variable to store stuff in later.
			// For now we'll initialize it with nothing.
			static ID: AtomicUsize = AtomicUsize::new(0);
			let match_container_var = format!("##match_tmp_var#{}", ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
			code.push(LLIR::Push(Value::None));
			code.push(LLIR::VarDecl(match_container_var.clone()));

			// Push the value of the expression onto the stack
			lower(&mut m.lhs, code, lambdas);

			// We generate a label corresponding with every branch
			let mut branch_labels = vec![];
			for _ in 0..m.arms.len() { branch_labels.push(gen_unique_label()); }
			// We also want to generate a label to jump to once we are done
			let done_label = gen_unique_label();
			// Also an error label.
			let err_label = gen_unique_label();
			branch_labels.push(err_label.clone());


			// Put a boolean onto the stack representing whether this matches or not
			fn gen_branch_match_code(p: &Pattern, container: &String, code: &mut Vec<LLIR>, lambdas: &mut HashMap<String, (Ast, Vec<String>)>) {
				match p {
					Pattern::Bool(b) => {
						// The value to match is on top of the stack. 'Eq' pops 2 values;
						// thus, we must duplicate in order to not remove that value.
						code.push(LLIR::Duplicate);
						code.push(LLIR::Push(Value::Bool(b.val)));
						code.push(LLIR::Eq);
					}
					Pattern::Unit => {
						code.push(LLIR::Duplicate);
						code.push(LLIR::Push(Value::None));
						code.push(LLIR::Eq);
					}
					Pattern::Num(n) => {
						// The value to match is on top of the stack. 'Eq' pops 2 values;
						// thus, we must duplicate in order to not remove that value.
						code.push(LLIR::Duplicate);
						code.push(LLIR::Push(Value::Num(n.val)));
						code.push(LLIR::Eq);
					}
					Pattern::Any(a) => {
						// Declare a variable with the desired value. This always matches, so push true.
						// We must duplicate because otherwise, VarDecl would pop the matching value
						// from the stack.
						code.push(LLIR::Duplicate);
						code.push(LLIR::VarDecl(a.name.clone()));
						code.push(LLIR::Push(Value::Bool(true)));
					}
					Pattern::Adt(cons_id, _, p) => {
						let failure = gen_unique_label();
						let end = gen_unique_label();

						// We first deconstruct the variant to get the tag.
						code.push(LLIR::DeconsVariant);
						let tag = match &lambdas.get(&cons_id.name).expect(&format!("Name {} not in lambda map (compiler error)", cons_id.name)).0 {
							Ast::Constructor(c) => c.tag,
							_ => panic!("Item in lambda map is not a constructor (compiler error)")
						};
						// Push the tag onto the stack, and compare with the variant tag
						code.push(LLIR::Push(Value::Num(tag as f64)));
						code.push(LLIR::Eq);
						// If not equal, jump to the tag failure block
						code.push(LLIR::GotoIfFalse(failure.clone()));

						// Otherwise, the tags match. The item on the stack now is the data inside the Adt.
						// We recursively pattern match, leaving a boolean on top of the stack.
						gen_branch_match_code(p, container, code, lambdas);
						// If false, we goto failure
						code.push(LLIR::GotoIfFalse(failure.clone()));
						// Otherwise, we jump to the end.
						// We need to pop the Adt data from the stack as well.
						// We replace it with true to indicate the success.
						code.push(LLIR::Pop);
						code.push(LLIR::Push(Value::Bool(true)));
						code.push(LLIR::Goto(end.clone()));

						code.push(LLIR::Label(failure));
						// In both cases of jumping here, the data from the Adt is left on the stack.
						// We must pop it.
						code.push(LLIR::Pop);
						// Now, we push False to indicate our failure.
						code.push(LLIR::Push(Value::Bool(false)));

						code.push(LLIR::Label(end));
					}
					Pattern::Prod(p) => {
						// We first deconstruct the tuple. This will dump a bunch of stuff on the stack.
						code.push(LLIR::DeconsProd);
						// The best (only, really) way to get all this stuff off the stack
						// is to clean it in a little loop, which we should do before we finish.
						let cleanup_label = gen_unique_label();
						// We'll also a failure label.
						let f_label = gen_unique_label();

						let mut num_to_pop = 0;
						for pattern in p {
							// We run the pattern. We now have a bool on top of the stack.
							gen_branch_match_code(pattern, container, code, lambdas);

							// If false, jump to the false block.
							// The cleanup block will require the number of elements left to remove.
							// We'll save it in the container for now.
							code.push(LLIR::Push(Value::Num((p.len() - num_to_pop) as f64)));
							code.push(LLIR::Set(container.clone()));
							code.push(LLIR::GotoIfFalse(f_label.clone()));

							// Otherwise, we continue. We should remove the num_to_pop var from the stack.
							code.push(LLIR::Pop);
							num_to_pop += 1;
						}

						// If we made it here, then we haven't jumped to the false block yet.
						// The cleanup code expects a bool on top of the stack. We must have succeeded,
						// so we push true.
						// There are no items to clean up, because we matched all of them.
						code.push(LLIR::Push(Value::Num(0.0)));
						code.push(LLIR::Push(Value::Bool(true)));
						code.push(LLIR::Goto(cleanup_label.clone()));

						// Similar to the above code, but false this time.
						// The number of items to clean is stored in the container.
						code.push(LLIR::Label(f_label));
						code.push(LLIR::Load(container.clone()));
						code.push(LLIR::Push(Value::Bool(false)));
						code.push(LLIR::Goto(cleanup_label.clone()));

						// The first elem on the stack is the result. The next is the number of things to clean up.
						code.push(LLIR::Label(cleanup_label));
						// The first thing we need to do is store the result boolean.
						code.push(LLIR::Set(container.clone()));
						// The top of the stack is now the number of elements to clean. We can dynamically pop these.
						code.push(LLIR::DynPop);
						// Put the result back on the top of the stack, and we're done.
						code.push(LLIR::Load(container.clone()))
					}
				}
			}

			// We generate the code for each branch
			for i in 0..m.arms.len() {
				let arm = &mut m.arms[i];
				// Declare the label for this arm
				code.push(LLIR::Label(branch_labels[i].clone()));
				gen_branch_match_code(&arm.0, &match_container_var, code, lambdas);
				// If applicable, we also generate code for the guard
				if let Some(guard) = &mut arm.1 {
					// If the pattern is false, we don't even need to try the guard
					code.push(LLIR::GotoIfFalse(branch_labels[i + 1].clone()));
					// Generate the code for the guard. A bool will be on the stack now.
					lower(guard, code, lambdas);
				}
				// If this returns false, we jump straight to the next arm.
				code.push(LLIR::GotoIfFalse(branch_labels[i + 1].clone()));
				// Otherwise, we generate the body for the arm, then jump to the end of the match
				lower(&mut arm.2, code, lambdas);
				code.push(LLIR::Goto(done_label.clone()));
			}

			code.push(LLIR::Label(err_label));
			code.push(LLIR::RuntimeErr("No pattern matched data given".to_string()));

			code.push(LLIR::Label(done_label));
			// The stack should now have the value pushed by the match arm
			// and the expression we are matching over.
			// We will store our result in the tmp variable, pop the stack, then push our value again.
			code.push(LLIR::Set(match_container_var.clone()));
			code.push(LLIR::Pop);
			code.push(LLIR::Load(match_container_var))
		},
		Ast::Let(l) => {
			let ident = &cast!(l.id.as_ref(), Ast::Ident).name;
			// Push variable value to stack
			lower(&mut l.lhs, code, lambdas);
			// Declare variable
			code.push(LLIR::VarDecl(ident.clone()));
			lower(&mut l.rhs, code, lambdas);
		}
		Ast::Const(_) => panic!("All consts should have been lifted (compiler error)"),
		Ast::Data(d) => if let Some(right) = &mut d.rhs {
			lower(right, code, lambdas);
		},
		Ast::Constructor(_) => panic!("Cannot lower constructor"),
		Ast::BinOp(b) => {
			// Push lhs
			lower(&mut b.lhs, code, lambdas);
			// Push rhs
			lower(&mut b.rhs, code, lambdas);
			code.push(match b.op_ty {
				OpTy::NumAdd => LLIR::Add,
				OpTy::NumSub => LLIR::Sub,
				OpTy::NumDiv => LLIR::Div,
				OpTy::NumMul => LLIR::Mul,
				OpTy::NumMod => LLIR::Mod,
				OpTy::NumGt => LLIR::Gt,
				OpTy::NumGte => LLIR::Gte,
				OpTy::NumLt => LLIR::Lt,
				OpTy::NumLte => LLIR::Lte,
				OpTy::NumEq | OpTy::BoolEq => LLIR::Eq,
				OpTy::NumNeq | OpTy::BoolNeq => LLIR::Neq,
				OpTy::BoolOr => LLIR::Or,
				OpTy::BoolAnd => LLIR::And,
			})
		}
		Ast::NumLit(n) => code.push(LLIR::Push(Value::Num(n.val))),
		Ast::BoolLit(b) => code.push(LLIR::Push(Value::Bool(b.val))),
		Ast::UnitLit => code.push(LLIR::Push(Value::None)),
		Ast::ProdLit(p) => {
			// Gen items in reverse order
			for i in (0..p.items.len()).rev() {
				lower(&mut p.items[i], code, lambdas);
			}
			// Make the tuple
			code.push(LLIR::MkProd(p.items.len() as u64));
		}
		Ast::None => panic!("Cannot lower None (compiler error)"),
	}
}

/// The main purpose of this data structure is to keep track
/// of which variables are captured in which scopes.
struct CapturedVarManager {
	// Variables that are declared through let, match, etc
	declared_in_scope: Vec<HashSet<String>>,
	// Variables that were used in this scope, but not declared
	captured: HashSet<String>,
	// A stack of let names
	most_recent_let: Vec<String>,
	// A stack of fn names
	current_fn_name: Vec<String>,
}

impl CapturedVarManager {
	pub fn new() -> CapturedVarManager {
		CapturedVarManager {
			declared_in_scope: vec![HashSet::new()],
			captured: HashSet::new(),
			most_recent_let: vec!["".to_owned()],
			current_fn_name: vec![],
		}
	}
	/// Make a unique name for a function.
	fn make_name() -> String {
		static ID: AtomicUsize = AtomicUsize::new(0);
		format!("##Funtion__{}", ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
	}
	/// Enter a new function scope, and create a function name to match.
	pub fn push_scope(&mut self) {
		self.declared_in_scope.push(HashSet::new());
		self.current_fn_name.push(Self::make_name());
	}
	/// Exit a function scope.
	pub fn pop_scope(&mut self) {
		self.declared_in_scope.pop();
		self.current_fn_name.pop();
	}
	/// Get the name of the function that we are currently in.
	pub fn get_fn_name(&self) -> String {
		self.current_fn_name.last().expect("No current fn name (compiler error)").to_owned()
	}
	/// Register an argument for the current function.
	pub fn register_arg(&mut self, id: &str) {
		self.declared_in_scope.last_mut().expect("No last scope map (compiler error)").insert(id.to_owned());
	}
	/// Register a let variable, and declare it in scope
	pub fn start_let(&mut self, id: &str) {
		self.most_recent_let.push(id.to_owned());
		self.declared_in_scope.last_mut().expect("No last scope map (compiler error)").insert(id.to_owned());
	}
	/// Exit the let block
	pub fn end_let(&mut self) {
		self.most_recent_let.pop();
	}
	/// Indicate that the variable `id` was used. If it was *not* declared in this scope, and it is not the most
	/// recent `let` id, then it is added as a captured variable.
	pub fn var_usage(&mut self, id: &str) {
		if !self.declared_in_scope.last().expect("No last scope map (compiler error)").contains(id)
			&& self.most_recent_let.last().expect("No most recent let (compiler error)") != id {
			self.captured.insert(id.to_owned());
		}
	}
	/// Find out whether this variable usage is recursive or not. If so, return the
	/// name of the function we are calling.
	pub fn is_recursive_call(&self, id: &str) -> Option<String> {
		if self.most_recent_let.last().expect("No most recent let (compiler error)") == id {
			if let Some(name) = self.current_fn_name.last() {
				Some(name.clone())
			} else {
				None
			}
		} else {
			None
		}
	}
	/// To be called directly after compiling a function; retrieves all the captured variables of that function.
	/// Additionally, this updates the captured variable set based on the now current scope's declared variables.
	pub fn get_captured(&mut self) -> Vec<String> {
		let last = self.declared_in_scope.last().expect("No last scope map (compiler error)");
		let result = self.captured.iter().map(|x| x.clone()).collect::<Vec<String>>();
		for item in last.iter() {
			self.captured.remove(item);
		}
		result
	}
}

/// Given an ast, construct a map from FnNames to a pair of (Abstraction, Captured Variables)
fn lambda_lift(ast: &mut Ast, map: &mut HashMap<String, (Ast, Vec<String>)>, c: &mut CapturedVarManager) {
	// Most of the stuff here is pretty straightforward -- just recurse over child nodes.
	match ast {
		Ast::Module(m) => {
			for node in m { lambda_lift(node, map, c); }
		}
		Ast::Appl(a) => {
			lambda_lift(&mut a.left, map, c);
			lambda_lift(&mut a.right, map, c);
		}
		Ast::TAppl(t) => lambda_lift(&mut t.left, map, c),
		Ast::Extern(e) => {
			for a in &mut e.args {
				lambda_lift(a, map, c)
			}
		}
		Ast::Abst(a) => {
			let id = cast!(a.ident.as_ref(), Ast::Ident);
			let tok = id.token.clone();

			// Enter the function scope, and register the argument
			c.push_scope();
			c.register_arg(&id.name);
			lambda_lift(&mut a.body, map, c);
			let name = c.get_fn_name();
			c.pop_scope();

			// Get the captured variables for the function we just compiled
			let captured = c.get_captured();

			// Replace our abstraction with a function call to the new top level function
			let abst = std::mem::replace(ast, Ast::Ident(Ident {
				kind: Type::None,
				name: name.clone(),
				token: tok,
			}));
			map.insert(name, (abst, captured));
		}
		Ast::TAbst(t) => lambda_lift(&mut t.body, map, c),
		Ast::Ident(i) => {
			if let Some(new_id) = c.is_recursive_call(&i.name) {
				// If this is a recursive call, we need to update the call to the new generated function name
				i.name = new_id;
			} else {
				c.var_usage(&i.name)
			}
		}
		Ast::If(i) => {
			lambda_lift(&mut i.condition, map, c);
			lambda_lift(&mut i.t_stmt, map, c);
			lambda_lift(&mut i.f_stmt, map, c);
		}
		Ast::Match(m) => {
			lambda_lift(&mut m.lhs, map, c);
			for arm in &mut m.arms {
				if let Some(guard) = arm.1.as_mut() {
					lambda_lift(guard, map, c);
				}
				lambda_lift(&mut arm.2, map, c);
			}
		},
		Ast::Let(l) => {
			let id = cast!(l.id.as_ref(), Ast::Ident);
			c.start_let(&id.name);
			lambda_lift(&mut l.lhs, map, c);
			lambda_lift(&mut l.rhs, map, c);
			c.end_let();
		}
		Ast::Const(co) => {
			let name = cast!(co.id.as_ref(), Ast::Ident).name.clone();
			let abst = match co.rhs.as_mut() {
				Ast::Abst(a) => a,
				_ => panic!("The body of a Const should be an Abst (compiler error)")
			};
			let arg_id = cast!(abst.ident.as_ref(), Ast::Ident);

			c.push_scope();
			c.register_arg(&arg_id.name);
			lambda_lift(&mut abst.body, map, c);
			c.pop_scope();
			let captured = c.get_captured();

			// This is almost identical to Abstractions, except we don't replace them with calls, because
			// they are already top level.

			let co = std::mem::replace(ast, Ast::None);
			map.insert(name, (co, captured));
		}
		Ast::Data(d) => {
			let mut count = 0;
			// The main idea is just to declare constructors as top level functions.
			// This is also a good place to finalize the tag for the data variant.
			for cons in &mut d.constructors {
				let cons_casted = cast!(cons, Ast::Constructor);
				cons_casted.tag = count;
				let id = cast!(cons_casted.id.as_ref(), Ast::Ident).name.clone();
				let node = std::mem::replace(cons, Ast::None);
				map.insert(id, (node, vec![]));
				count += 1;
			}
			if let Some(right) = &mut d.rhs {
				lambda_lift(right, map, c);
			}
		}
		Ast::BinOp(b) => {
			lambda_lift(&mut b.lhs, map, c);
			lambda_lift(&mut b.rhs, map, c);
		}
		Ast::Constructor(_) => panic!("Cannot lambda lift constructor"),
		Ast::NumLit(_) | Ast::BoolLit(_) | Ast::UnitLit => (),
		Ast::ProdLit(p) => {
			for item in &mut p.items {
				lambda_lift(item, map, c);
			}
		}
		Ast::None => (),
	}
}


