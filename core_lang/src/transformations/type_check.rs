use std::collections::HashMap;

use crate::ast::*;
use crate::errors::*;

/// Make sure that types `a` and `b` are compatible.
fn ensure_match(a: &Type, b: &Type) -> Res<()> {
	let emsg = Err(ErrMsg::new(&format!(
		"Types {} and {} are incompatible",
		a.to_str(),
		b.to_str()
	)));
	match (a, b) {
		// Simple types are compatible with each other.
		(Type::Bool, Type::Bool) | (Type::Num, Type::Num) | (Type::Unit, Type::Unit) => Ok(()),
		// Type variables are compatible if they have the same name.
		(Type::TypeVar(_a), Type::TypeVar(_b)) => Ok(()),
		// Functions are compatible if their left and right sides match.
		(Type::Fn(a), Type::Fn(b)) => {
			ensure_match(&a.lhs, &b.lhs)?;
			ensure_match(&a.rhs, &b.rhs)
		}
		// Products are compatible if all of their arguments are the same.
		(Type::Prod(a), Type::Prod(b)) => {
			if a.items.len() != b.items.len() {
				return emsg;
			}
			for i in 0..a.items.len() {
				ensure_match(&a.items[i], &b.items[i])?;
			}
			Ok(())
		}
		// Adts are the same if their names and arguments all match.
		(Type::Adt(a), Type::Adt(b)) => {
			let id1 = &cast!(a.id.as_ref(), Ast::Ident).name;
			let id2 = &cast!(b.id.as_ref(), Ast::Ident).name;

			if id1 != id2 {
				return emsg;
			}
			if a.args.len() != b.args.len() {
				return emsg;
			}

			for i in 0..a.args.len() {
				ensure_match(&a.args[i], &b.args[i])?;
			}

			Ok(())
		}
		// Universal types are the same if their arguments and bodies match.
		(Type::Universal(a), Type::Universal(b)) => {
			ensure_match(&a.arg, &b.arg)?;
			ensure_match(&a.body, &b.body)
		}
		_ => emsg,
	}
}

/// Insert a key-value pair into the mapping from identifiers to types.
fn map_ins(id: &mut Ast, ty: Type, map: &mut HashMap<String, Type>) {
	let id = cast!(id, Ast::Ident);
	id.kind = ty.clone();
	match map.insert(id.name.clone(), ty) {
		Some(_) => panic!(
			"Name {} already in type map (this is a compiler error)",
			id.name
		),
		_ => (),
	}
}

/// Get the type of an identifier from the map, and update the identifier to match.
fn map_check(id: &mut Ast, map: &mut HashMap<String, Type>) {
	let id = cast!(id, Ast::Ident);
	match map.get(&id.name) {
		Some(t) => id.kind = t.clone(),
		_ => panic!(
			"Name {} not in type map (this is a compiler error)",
			id.name
		),
	}
}

/// Make sure all types are consistent in an Ast.
pub fn type_check(ast: &mut Ast, map: &mut HashMap<String, Type>) -> Res<Type> {
	match ast {
		// For modules, we want to make sure we declare data first, then constructors & const functions & externs.
		// That way, we can reference an identifier from anywhere in the code without worrying about
		// whether it was defined above us or not.
		Ast::Module(m) => {
			// Insert just data definitions
			for node in m.iter_mut() {
				if let Ast::Data(d) = node {
					let ty = Type::Adt(Adt {
						id: d.id.clone(),
						args: d.args.clone()
					});
					map_ins(&mut d.id, ty, map);
				}
			}
			// Now do data constructors, const idents, and externs
			for node in m.iter_mut() {
				if let Ast::Data(_) = node {
					type_check(node, map)?;
				} else if let Ast::Const(c) = node {
					map_ins(&mut c.id, c.hint.clone(), map);
				}
			}
			// Do everything else
			for node in m {
				if let Ast::Data(_) = node { continue; }
				if let Ast::Extern(_) = node { continue; }
				type_check(node, map)?;
			}
			Ok(Type::None)
		}
		Ast::Extern(e) => {
			for a in &mut e.args {
				type_check(a, map)?;
			}
			Ok(e.ret_ty.clone())
		}
		Ast::Appl(a) => {
			// Check the left and right sides.
			// Ensure the right side is compatible with the arg of the left side.
			// Return the right side of the function type.
			let lhs = type_check(&mut a.left, map)?;
			let rhs = type_check(&mut a.right, map)?;
			if let Type::Fn(f) = lhs {
				ensure_match(&f.lhs, &rhs)?;
				a.kind = f.rhs.as_ref().clone();
				Ok(f.rhs.as_ref().clone())
			} else {
				Err(ErrMsg::new(&format!(
					"Non-function type {} cannot be called",
					lhs.to_str()
				)))
			}
		}
		Ast::TAppl(t) => {
			// We make sure the left side is a type function
			let lhs = type_check(&mut t.left, map)?;
			if let Type::Universal(mut u) = lhs {
				// Get the argument
				let arg = match u.arg.as_ref() {
					Type::TypeVar(tv) => tv,
					_ => panic!(
						"Could not convert Universal arg to TypeVar (this is a compiler error)"
					),
				};
				// Get the new type
				apply_type(&arg.id, &mut u.body, &t.right);
				// The type of the whole expression is the return type of the type function
				t.kind = u.body.as_ref().clone();
				Ok(u.body.as_ref().clone())
			} else {
				Err(ErrMsg::new(&format!(
					"Non-function type {} cannot be called",
					lhs.to_str()
				)))
			}
		}
		Ast::Abst(a) => {
			// Get the type of the argument and the body, and arrange into a function type.
			map_ins(&mut a.ident, a.type_hint.clone(), map);
			let lhs = type_check(&mut a.body, map)?;
			let fnty = Type::Fn(Fn {
				lhs: Box::new(a.type_hint.clone()),
				rhs: Box::new(lhs),
			});
			a.kind = fnty.clone();
			Ok(fnty)
		}
		Ast::TAbst(t) => {
			// No need to add the type variable to the map. It should be filled later anyway.
			let body_ty = type_check(&mut t.body, map)?;
			let univ_ty = Type::Universal(Universal {
				arg: Box::new(t.ident.clone()),
				body: Box::new(body_ty),
			});
			t.kind = univ_ty.clone();
			Ok(univ_ty)
		}
		Ast::Ident(_) => {
			// Just make sure our ident is in here
			map_check(ast, map);
			let i = cast!(ast, Ast::Ident);
			Ok(i.kind.clone())
		}
		Ast::If(i) => {
			// Make sure the condition is a bool, and that the rhs and lhs match
			let cty = type_check(&mut i.condition, map)?;
			ensure_match(&cty, &Type::Bool)?;
			let t1 = type_check(&mut i.t_stmt, map)?;
			let t2 = type_check(&mut i.f_stmt, map)?;
			ensure_match(&t1, &t2)?;
			i.kind = t1;
			Ok(t2)
		}
		Ast::Match(m) => {
			// Make sure the patterns match with the expression type.
			// Make sure all the arms are the same type.
			let expr_type = type_check(&mut m.lhs, map)?;
			let mut curr_arm_ty = None;
			for arm in &mut m.arms {
				compare_to_pattern(&mut arm.0, &expr_type, map)?;
				let ty = type_check(&mut arm.2, map)?;

				if let Some(guard) = arm.1.as_mut() {
					let guard_ty = type_check(guard, map)?;
					ensure_match(&guard_ty, &Type::Bool)?;
				}

				if let Some(arm_ty) = curr_arm_ty.as_ref() {
					ensure_match(arm_ty, &ty)?;
				} else { curr_arm_ty = Some(ty); }
			}

			if let Some(t) = curr_arm_ty {
				m.kind = t.clone();
				Ok(t)
			} else {
				panic!("Match has no arms (this is a compiler error)")
			}
		},
		Ast::Let(l) => {
			// Insert the identifier into the map. Check the right and left.
			map_ins(&mut l.id, l.type_hint.clone(), map);
			let lhs = type_check(&mut l.lhs, map)?;
			ensure_match(&lhs, &l.type_hint)?;
			let rhs = type_check(&mut l.rhs, map)?;
			l.kind = rhs.clone();
			Ok(rhs)
		}
		Ast::Const(c) => {
			// Check the body of the const block.
			// c.id should have already been added by the initial scan
			let rhs_ty = type_check(&mut c.rhs, map)?;
			ensure_match(&rhs_ty, &c.hint)?;
			c.kind = Type::None;
			Ok(Type::None)
		}
		Ast::BinOp(b) => {
			// Make sure the right and left sides are compatible with the operator specified
			let t1 = type_check(&mut b.lhs, map)?;
			let t2 = type_check(&mut b.rhs, map)?;

			match b.op_ty {
				OpTy::NumAdd
				| OpTy::NumSub
				| OpTy::NumMul
				| OpTy::NumDiv
				| OpTy::NumMod => {
					ensure_match(&t1, &Type::Num)?;
					ensure_match(&t2, &Type::Num)?;
					Ok(Type::Num)
				}
				OpTy::NumEq
				| OpTy::NumNeq
				| OpTy::NumGt
				| OpTy::NumGte
				| OpTy::NumLt
				| OpTy::NumLte => {
					ensure_match(&t1, &Type::Num)?;
					ensure_match(&t2, &Type::Num)?;
					Ok(Type::Bool)
				}
				OpTy::BoolEq
				| OpTy::BoolNeq
				| OpTy::BoolAnd
				| OpTy::BoolOr => {
					ensure_match(&t1, &Type::Bool)?;
					ensure_match(&t2, &Type::Bool)?;
					Ok(Type::Bool)
				}
			}
		}
		Ast::Data(d) => {
			// For each constructor, make a function type that accepts all type arguments, then the
			// value argument, and returns the Data type.

			// For example, given
			//      data Tree a
			//          | Leaf a
			//          | Branch (Tree a, Tree a)
			// we construct types
			//      Leaf :: forall a . a -> Tree a
			//      Branch :: forall a . (Tree a, Tree a) -> Tree a


			let cons_output = Type::Adt(Adt {
				id: d.id.clone(),
				args: d.args.clone(),
			});

			// I know clever code is usually bad, and I typically favor readability, but the readable code is a lot longer.
			// Basically what we are doing is this:
			//      - If the constructor has no type args, return a simple function type
			//      - Otherwise, construct a function type that accepts all type args, then the value, then spits out the type of the Data
			for item in &mut d.constructors {
				let cons = cast!(item, Ast::Constructor);
				let cons_ty = if d.args.is_empty() {
					Type::Fn(Fn {
						rhs: Box::new(cons_output.clone()),
						lhs: Box::new(cons.data_type.clone()),
					})
				} else {
					let mut iter = d.args.iter();
					let init = iter.next_back().unwrap();
					iter.rfold(
						Type::Universal(Universal {
							arg: Box::new(init.clone()),
							body: Box::new(Type::Fn(Fn {
								rhs: Box::new(cons_output.clone()),
								lhs: Box::new(cons.data_type.clone()),
							})),
						}),
						|acc, x| {
							Type::Universal(Universal {
								arg: Box::new(x.clone()),
								body: Box::new(acc),
							})
						},
					)
				};

				// Insert the constructor type into the map
				map_ins(&mut cons.id, cons_ty, map);
			}

			let rhs = if let Some(right) = &mut d.rhs {
				type_check(right, map)?
			} else { Type::None };
			d.kind = rhs.clone();
			Ok(rhs)
		}
		Ast::Constructor(_) => panic!("Cannot type check constructor (this is a compiler error)"),
		Ast::NumLit(_) => Ok(Type::Num),
		Ast::BoolLit(_) => Ok(Type::Bool),
		Ast::UnitLit => Ok(Type::Unit),
		Ast::ProdLit(p) => {
			let mut items = vec![];
			for expr in &mut p.items {
				items.push(type_check(expr, map)?);
			}
			Ok(Type::Prod(Prod { items }))
		}
		Ast::None => panic!("Cannot typecheck none node (this is a compiler error)"),
	}
}

/// Ensure that a given type is compatible with a given pattern
fn compare_to_pattern(p: &mut Pattern, t: &Type, m: &mut HashMap<String, Type>) -> Res<()> {
	match (p, t) {
		(Pattern::Num(_), Type::Num) => Ok(()),
		(Pattern::Unit, Type::Unit) => Ok(()),
		(Pattern::Bool(_), Type::Bool) => Ok(()),
		(Pattern::Any(i), t) => {
			// If an identifier is matching against a type, we should register that this identifier must be that type.
			i.kind = t.clone();
			map_ins(&mut Ast::Ident(i.clone()), t.clone(), m);
			Ok(())
		}
		(Pattern::Adt(cons_id, args, p), Type::Adt(_)) => {
			// We need to make sure that the argument types & lengths match,
			// and that the constructor is for the correct Adt. We can then match
			// the inner data.


			let cons_ty = match m.get_mut(&cons_id.name) {
				Some(t) => t,
				_ => panic!("Constructor {} either doesn't exist or isn't a function (compiler error)", cons_id.name)
			};

			fn collect_type_args(a: &mut Vec<String>, f: &mut Type) {
				if let Type::Universal(u) = f {
					if let Type::TypeVar(tv) = u.arg.as_ref() {
						a.push(tv.id.clone());
						collect_type_args(a, &mut u.body);
					}
				}
			}

			let mut type_args = vec![];
			collect_type_args(&mut type_args, cons_ty);

			if args.len() != type_args.len() {
				return Err(ErrMsg::new(&format!("Constructor {} takes {} args ({} supplied)", cons_id.name, type_args.len(), args.len())))
			}

			for i in 0..args.len() {
				apply_type(&type_args[i], cons_ty, &args[i]);
			}

			fn get_last_args(t: &Type) -> (Type, Type) {
				if let Type::Universal(u) = t {
					if let Type::TypeVar(_) = u.arg.as_ref() {
						return get_last_args(&u.body);
					}
				} else if let Type::Fn(f) = t {
					return (f.lhs.as_ref().clone(), f.rhs.as_ref().clone())
				}
				panic!("Last args of constructor should be data and ADT (compiler error)")
			}

			let (data_arg, adt_ty) = get_last_args(&cons_ty);

			// Ensure that the ADT here is compatible with the ADT we are matching on
			ensure_match(&adt_ty, t)?;

			// Finally, we are able to match our pattern to the inner data of the AST
			compare_to_pattern(p, &data_arg, m)
		}
		(Pattern::Prod(args), Type::Prod(p)) => {
			// We just need to make sure the pattern arg types match the product type argument types.

			if args.len() != p.items.len() {
				return Err(ErrMsg::new(&format!("Product pattern should have {} args ({} specified)", p.items.len(), args.len())))
			}

			for i in 0..args.len() {
				compare_to_pattern(&mut args[i], &p.items[i], m)?;
			}

			Ok(())
		}
		_ => Err(ErrMsg::new(&format!("Patten is incompatible with type {}", t.to_str())))
	}
}

/// Perform a type application, substituting a type variable for a concrete variable inside some type expression.
fn apply_type(arg_id: &str, body: &mut Type, ty: &Type) {
	// The only part of interest here is TypeVar, which is
	// substituted for a concrete type iff the name matches the argument specified.
	match body {
		Type::Bool | Type::Kind | Type::None | Type::Num | Type::Unit => (),
		Type::Adt(a) => {
			for arg in &mut a.args {
				apply_type(arg_id, arg, ty);
			}
		}
		Type::Fn(f) => {
			apply_type(arg_id, &mut f.lhs, ty);
			apply_type(arg_id, &mut f.rhs, ty)
		}
		Type::Prod(p) => {
			for item in &mut p.items {
				apply_type(arg_id, item, ty)
			}
		}
		Type::TypeVar(tv) => {
			if tv.id == arg_id {
				*body = ty.clone();
			}
		}
		Type::Universal(u) => apply_type(arg_id, &mut u.body, ty),
	}
}
