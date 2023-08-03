#![allow(unused)]

use std::{
	collections::HashMap,
	sync::atomic::{AtomicUsize, Ordering},
};

use crate::lexer::Token;
use crate::{ast::*, errors::*};

/*
	This code is not part of the compiler pipeline. A more complex, namespace-aware
	renamer is present in earlier stages, rendering this renamer unnecessary. However,
	the code will remain here if there is ever a need to compile Core code.
*/


fn gen_name(name: &str) -> String {
	static COUNT: AtomicUsize = AtomicUsize::new(0);
	format!("{}#{}", name, COUNT.fetch_add(1, Ordering::Relaxed))
}

/// A simple mapping from the local identifiers in a scope to their unique names
struct NameManager {
	scopes: Vec<HashMap<String, String>>,
}

impl NameManager {
	pub fn new() -> NameManager {
		NameManager {
			scopes: vec![HashMap::new()],
		}
	}
	fn last_scope(&mut self) -> &mut HashMap<String, String> {
		self.scopes
			.last_mut()
			.expect("Scopes is empty (this is a compiler error)")
	}
	pub fn push_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}
	pub fn pop_scope(&mut self) { self.scopes.pop(); }
	pub fn add_scoped_id(&mut self, orig_name: &str) -> String {
		self.push_scope();
		let name = gen_name(orig_name);
		self.last_scope().insert(orig_name.to_owned(), name.clone());
		name
	}
	pub fn add_id(&mut self, orig_name: &str) -> Res<String> {
		let name = gen_name(orig_name);
		match self.last_scope().insert(orig_name.to_string(), name.clone()) {
			None => Ok(name),
			_ => Err(ErrMsg::new(&format!("Identifier {} was already declared in this scope", orig_name)))
		}
	}
	pub fn get_id(&self, orig_name: &str, tok: &Token) -> Res<String> {
		let mut place = self.scopes.len() - 1;
		loop {
			match self.scopes[place].get(orig_name) {
				Some(name) => return Ok(name.clone()),
				_ => (),
			}
			if place == 0 {
				break;
			}
			place -= 1;
		}
		Err(ErrMsg::new(&format!(
			"Identifier {} was not declared in this scope",
			orig_name
		))
			.place(tok.line, tok.col))
	}
}

pub fn rename(ast: &mut Ast) -> Res<()> {
	let mut nm = NameManager::new();
	rename_rec(ast, &mut nm)
}

fn rename_rec(ast: &mut Ast, nm: &mut NameManager) -> Res<()> {
	match ast {
		Ast::Module(m) => {
			for node in m { rename_rec(node, nm)? }
			Ok(())
		}
		Ast::Appl(a) => {
			rename_rec(&mut a.left, nm)?;
			rename_rec(&mut a.right, nm)
		}
		Ast::Extern(e) => {
			todo!()
		}
		Ast::TAppl(t) => {
			rename_rec(&mut t.left, nm)?;
			rename_type(&mut t.right, nm)
		}
		Ast::Abst(a) => {
			let ident = cast!(a.ident.as_mut(), Ast::Ident);
			let new_name = nm.add_scoped_id(&ident.name);
			ident.name = new_name;
			rename_type(&mut a.type_hint, nm)?;
			rename_rec(a.body.as_mut(), nm)?;
			nm.pop_scope();
			Ok(())
		}
		Ast::TAbst(t) => {
			if let Type::TypeVar(tv) = &mut t.ident {
				let new_name = nm.add_scoped_id(&tv.id);
				tv.id = new_name;
				rename_rec(&mut t.body, nm)?;
				nm.pop_scope();
				Ok(())
			} else {
				panic!("Could not convert TAbst id to TypeVar (this is a compiler error)")
			}
		}
		Ast::Let(l) => {
			let ident = cast!(l.id.as_mut(), Ast::Ident);
			// If the variable is initialized to a function, we allow recursion.
			// Otherwise, the name shouldn't appear in the definition.
			if let Ast::Abst(_) = l.lhs.as_ref() {
				let new_name = nm.add_scoped_id(&ident.name);
				ident.name = new_name;
				rename_type(&mut l.type_hint, nm)?;
				rename_rec(&mut l.lhs, nm)?;
			} else {
				rename_type(&mut l.type_hint, nm)?;
				rename_rec(&mut l.lhs, nm)?;
				let new_name = nm.add_scoped_id(&ident.name);
				ident.name = new_name;
			}
			rename_rec(&mut l.rhs, nm)?;
			nm.pop_scope();
			Ok(())
		}
		Ast::Const(c) => {
			let ident = cast!(c.id.as_mut(), Ast::Ident);

			// Recursion only allowed with functions
			if let Ast::Abst(_) = c.rhs.as_ref() {
				// Declare name in this current scope
				let new_name = nm.add_id(&ident.name)?;
				ident.name = new_name;
				rename_type(&mut c.hint, nm)?;
				nm.push_scope();
				rename_rec(&mut c.rhs, nm)?;
				nm.pop_scope();
				Ok(())
			} else {
				// Rename body, then declare
				rename_type(&mut c.hint, nm)?;
				nm.push_scope();
				rename_rec(&mut c.rhs, nm)?;
				nm.pop_scope();

				let new_name = nm.add_id(&ident.name)?;
				ident.name = new_name;
				Ok(())
			}
		}
		Ast::Data(d) => {
			let toplvl = d.rhs.is_none();

			let ident = cast!(d.id.as_mut(), Ast::Ident);

			if toplvl {
				let new_name = nm.add_id(&ident.name)?;
				ident.name = new_name;
				nm.push_scope();
			} else {
				let new_name = nm.add_scoped_id(&ident.name);
				ident.name = new_name.clone();
			}

			for arg in &mut d.args {
				let arg_id = cast!(arg, Type::TypeVar);
				let new_arg_name = nm.add_id(&arg_id.id)?;
				arg_id.id = new_arg_name;
			}

			for cons in &mut d.constructors {
				let cons = cast!(cons, Ast::Constructor);
				rename_type(&mut cons.kind, nm)?;
				rename_type(&mut cons.data_type, nm)?;
			}

			nm.pop_scope();

			// We want to declare constructors in the same scope as the Data node.

			for cons in &mut d.constructors {
				let cons = cast!(cons, Ast::Constructor);
				let id = cast!(cons.id.as_mut(), Ast::Ident);
				let new_name = nm.add_id(&id.name)?;
				id.name = new_name;
			}

			if let Some(rhs) = &mut d.rhs {
				nm.push_scope();
				rename_rec(rhs, nm)?;
				nm.pop_scope();
			}
			Ok(())
		}
		Ast::Constructor(c) => {
			rename_type(&mut c.kind, nm)?;
			let id = cast!(c.id.as_mut(), Ast::Ident);
			let new_name = nm.add_id(&id.name)?;
			id.name = new_name;
			rename_type(&mut c.data_type, nm)
		}
		Ast::Ident(i) => {
			i.name = nm.get_id(&i.name, &i.token)?;
			Ok(())
		}
		Ast::If(i) => {
			rename_rec(i.condition.as_mut(), nm)?;
			rename_rec(i.t_stmt.as_mut(), nm)?;
			rename_rec(i.f_stmt.as_mut(), nm)
		}
		Ast::Match(m) => {
			fn add_pattern_args(p: &mut Pattern, nm: &mut NameManager) -> Res<()> {
				match p {
					Pattern::Num(_) | Pattern::Bool(_) | Pattern::Unit => Ok(()),
					Pattern::Any(i) => {
						i.name = nm.add_id(&i.name)?;
						Ok(())
					}
					Pattern::Adt(ident, args, a) => {
						ident.name = nm.get_id(&ident.name, &ident.token)?;
						add_pattern_args(a, nm)?;
						for arg in args {
							rename_type(arg, nm)?;
						}
						Ok(())
					}
					Pattern::Prod(p) => {
						for item in p {
							add_pattern_args(item, nm)?;
						}
						Ok(())
					}
				}
			}

			rename_rec(&mut m.lhs, nm)?;
			for arm in &mut m.arms {
				nm.push_scope();
				add_pattern_args(&mut arm.0, nm)?;
				if let Some(guard) = &mut arm.1 { rename_rec(guard, nm)?; }
				rename_rec(&mut arm.2, nm)?;
				nm.pop_scope();
			}
			Ok(())
		},
		Ast::NumLit(_) | Ast::BoolLit(_) | Ast::UnitLit => Ok(()),
		Ast::ProdLit(s) => {
			for item in &mut s.items {
				rename_rec(item, nm)?;
			}
			Ok(())
		}
		Ast::BinOp(b) => {
			rename_rec(&mut b.lhs, nm)?;
			rename_rec(&mut b.rhs, nm)
		}
		Ast::None => panic!("None should not appear here (compiler error)"),
	}
}

fn rename_type(t: &mut Type, nm: &mut NameManager) -> Res<()> {
	match t {
		Type::None | Type::Bool | Type::Num | Type::Kind | Type::Unit => Ok(()),
		Type::TypeVar(i) => {
			i.id = nm.get_id(&i.id, &i.tok)?;
			Ok(())
		}
		Type::Fn(f) => {
			rename_type(&mut f.lhs, nm)?;
			rename_type(&mut f.rhs, nm)
		}
		Type::Prod(p) => {
			for item in &mut p.items {
				rename_type(item, nm)?;
			}
			Ok(())
		}
		Type::Adt(a) => {
			rename_rec(&mut a.id, nm)?;
			for arg in &mut a.args {
				rename_type(arg, nm)?;
			}
			Ok(())
		}
		Type::Universal(u) => {
			let id = match u.arg.as_mut() {
				Type::TypeVar(tv) => tv,
				_ => {
					panic!("Could not convert Universal arg to TypeVar (this is a compiler error)")
				}
			};
			let new_name = nm.add_scoped_id(&id.id);
			id.id = new_name;
			rename_type(&mut u.body, nm)
		}
	}
}
