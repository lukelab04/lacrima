use std::sync::atomic::AtomicUsize;
use crate::ast::*;
use core_lang::ast;

/// Convert an AstNode into the lower Ast.
pub fn to_lower_ast(ast: &mut AstNode) -> ast::Ast {
	match ast {
		// Modules are pretty simple, because they correspond 1-1 with the ast::Module node.
		// Just plug & chug.
		AstNode::Module(m) => {
			let mut nodes = vec![];
			for item in m {
				nodes.push(to_lower_ast(item));
			}
			ast::Ast::Module(nodes)
		}
		// Named functions are a little tricky. They correspond roughly to the ast::Const node.
		// The Const node expects an ast::Abst (a lambda function) as the body. The tricky part here is that ast::Abst
		// only takes a single argument, whereas our named function may take many. Thus, we must curry our
		// function, changing both the body and the types to fit.
		//
		// As an example, the function
		//      fn example (a: Num, b: Num): Num => a + b
		// would be converted to
		//      (a: Num): Num -> Num =>
		//          (b: Num): Num => a + b
		//
		AstNode::NamedFn(n) => {
			// First, we convert the type of our function into a core type.
			// As an example, 'extract_lambda_type' turns the signature (a: Num, b: Num): Num into (Num -> Num -> Num),
			// which is then further lowered into the core type.
			let ty = match n.lambda.as_mut() {
				AstNode::Lambda(l) => type_to_core_lang_type(&mut extract_lambda_type(l)),
				_ => panic!("Fn body is not a lambda (compiler error"),
			};

			// Most of the hard work described above is handled by lowering the lambda.
			ast::Ast::Const(ast::Const {
				fully_qualified_name: n.qualified_name.as_ref().map(|x| x.to_str()),
				kind: ast::Type::None,
				id: Box::new(to_lower_ast(&mut n.id)),
				hint: ty,
				rhs: Box::new(to_lower_ast(&mut n.lambda)),
			})
		}
		// Externs, like named functions, must be curried. The 'ast::Extern' node represents a call to an external
		// function that takes a certain number of arguments, which is somewhat different than our treatment
		// of Externs here. Thus, we must curry, and replace the very inner expression with an ast::Extern node.
		AstNode::Extern(e) => {
			// The main idea behind this function is to take a function type (like Num -> Num -> Bool) and translate
			// it into a series of nested lambda functions. To accomplish this, we walk down the type tree, creating
			// lambdas and variables as necessary.

			// For example, the type (Num -> Num -> Bool) would be translated into
			//      (x1: Num): Num -> Bool =>
			//          (x2: Num): Bool =>
			//              externCall x1 x2
			// where x1, x2 are newly generated names.
			fn ty_to_lambdas(t: &Type, args: &mut Vec<ast::Ast>, tok: crate::lexer::Token, name: String) -> ast::Ast {
				static COUNT: AtomicUsize = AtomicUsize::new(0);

				// If we are at a function type, we can curry further.
				// Create an abstraction and recursively lower the right hand side of the function type.
				if let Type::FnType(fty) = t {
					let new_id = ast::Ast::Ident(ast::Ident {
						kind: ast::Type::None,
						name: format!("##Extern_Created_Ident{}", COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)),
						token: tok.clone().to_core_tok()
					});
					args.push(new_id.clone());
					ast::Ast::Abst(ast::Abst {
						kind: ast::Type::None,
						type_hint: type_to_core_lang_type(&mut fty.lhs.clone()),
						ident: Box::new(new_id),
						body: Box::new(ty_to_lambdas(&fty.rhs, args, tok, name))
					})
				}

				// If we are at a non-fn type, we cannot curry any further.
				// Create a call to the extern function.
				else {
					ast::Ast::Extern(ast::Extern {
						id: Box::new(ast::Ast::Ident(ast::Ident {
							kind: ast::Type::None,
							name,
							token: tok.clone().to_core_tok()
						})),
						args: args.clone(),
						ret_ty: type_to_core_lang_type(&mut t.clone()),
					})
				}
			}
			let abst = ty_to_lambdas(&e.ty, &mut vec![], e.token.clone(), e.id.as_ident_immut().name.clone());

			ast::Ast::Const(ast::Const {
				fully_qualified_name: None,
				kind: ast::Type::None,
				id: Box::new(to_lower_ast(&mut e.id)),
				hint: type_to_core_lang_type(&mut e.ty),
				rhs: Box::new(abst)
			})
		}
		// Data has a 1:1 comparison with ast::Data.
		// Very simple and straightforward, we just plug and chug.
		AstNode::Data(d) => {
			// Collect type arguments for the data
			let mut args = vec![];
			for arg in &d.args {
				let arg = arg.as_ident_immut();
				args.push(ast::Type::TypeVar(ast::TypeVar {
					id: arg.name.clone(),
					tok: arg.token.clone().to_core_tok(),
				}));
			}

			// Collect the constructors
			let mut cons = vec![];
			for item in &mut d.constructors {
				let tag = cons.len();
				cons.push(ast::Ast::Constructor(ast::Constructor {
					id: Box::new(to_lower_ast(&mut item.0)),
					kind: ast::Type::None,
					data_type: type_to_core_lang_type(&mut item.1),
					tag: tag as u64,
				}))
			}

			ast::Ast::Data(ast::Data {
				id: Box::new(to_lower_ast(&mut d.id)),
				kind: ast::Type::None,
				args,
				constructors: cons,
				rhs: None,
			})
		}
		AstNode::Let(l) => ast::Ast::Let(ast::Let {
			kind: ast::Type::None,
			type_hint: type_to_core_lang_type(&mut l.hint),
			id: Box::new(to_lower_ast(&mut l.id)),
			lhs: Box::new(to_lower_ast(&mut l.lhs)),
			rhs: Box::new(to_lower_ast(&mut l.rhs)),
		}),
		AstNode::Ident(i) => ast::Ast::Ident(ast::Ident {
			kind: ast::Type::None,
			name: i.name.clone(),
			token: i.token.clone().to_core_tok(),
		}),
		AstNode::If(i) => ast::Ast::If(ast::If {
			kind: ast::Type::None,
			condition: Box::new(to_lower_ast(&mut i.condition)),
			t_stmt: Box::new(to_lower_ast(&mut i.t_stmt)),
			f_stmt: Box::new(to_lower_ast(&mut i.f_stmt)),
		}),
		// Match is almost 1:1, but we need to lower the patterns as well.
		// This is handled with the 'pattern_to_core_lang_pattern' function.
		// Other than that, it is a simple translation.
		AstNode::Match(m) => ast::Ast::Match(ast::Match {
			kind: ast::Type::None,
			lhs: Box::new(to_lower_ast(&mut m.expr)),
			arms: m
				.arms
				.iter_mut()
				.map(|(p, g, e)| {
					(
						pattern_to_core_lang_pattern(p),
						if let Some(g) = g {
							Some(to_lower_ast(g))
						} else {
							None
						},
						to_lower_ast(e),
					)
				})
				.collect(),
		}),
		AstNode::NumLit(f) => ast::Ast::NumLit(ast::NumLit {
			kind: ast::Type::Num,
			val: *f,
		}),
		AstNode::BoolLit(b) => ast::Ast::BoolLit(ast::BoolLit {
			kind: ast::Type::Bool,
			val: *b,
		}),
		AstNode::UnitLit => ast::Ast::UnitLit,
		AstNode::Tuple(t) => {
			let mut items = vec![];
			for item in t {
				items.push(to_lower_ast(item));
			}
			ast::Ast::ProdLit(ast::ProdLit { items })
		}
		// Lambdas are tricky. These lambdas can accept multiple arguments, while core lambdas
		// only accept 1. Therefore, we must curry our lambdas. We accomplish this by looking
		// at the number of arguments are lambda defines. If it is just 1, we can lower directly.
		// Otherwise, we must extract the first argument into a new abstraction, then lower
		// the rest of the body.
		AstNode::Lambda(l) => {
			if l.args.is_empty() {
				panic!("Lambda has no arguments (compiler error)")
			} else if l.args.len() == 1 {
				ast::Ast::Abst(ast::Abst {
					kind: ast::Type::None,
					type_hint: type_to_core_lang_type(&mut l.args[0].1),
					ident: Box::new(to_lower_ast(&mut l.args[0].0)),
					body: Box::new(to_lower_ast(&mut l.body)),
				})
			} else {
				let mut args = std::mem::replace(&mut l.args, vec![]);
				let ret_type = std::mem::replace(&mut l.ret_type, Type::Num);
				let body = std::mem::replace(&mut l.body, Box::new(AstNode::NumLit(0.0)));
				let mut first = args.remove(0);
				ast::Ast::Abst(ast::Abst {
					kind: ast::Type::None,
					type_hint: type_to_core_lang_type(&mut first.1),
					ident: Box::new(to_lower_ast(&mut first.0)),
					body: Box::new(to_lower_ast(&mut AstNode::Lambda(Lambda {
						args,
						ret_type,
						body,
					}))),
				})
			}
		}
		AstNode::PrefixOp(_) => todo!(),
		AstNode::BinOp(b) => {
			let op_ty = match b.op {
				OpType::Add => ast::OpTy::NumAdd,
				OpType::Sub => ast::OpTy::NumSub,
				OpType::Div => ast::OpTy::NumDiv,
				OpType::Mul => ast::OpTy::NumMul,
				OpType::Mod => ast::OpTy::NumMod,
				OpType::Eq => ast::OpTy::NumEq,
				OpType::NEq => ast::OpTy::NumNeq,
				OpType::And => ast::OpTy::BoolAnd,
				OpType::Or => ast::OpTy::BoolOr,
				OpType::Gt => ast::OpTy::NumGt,
				OpType::Gte => ast::OpTy::NumGte,
				OpType::Lt => ast::OpTy::NumLt,
				OpType::Lte => ast::OpTy::NumLte,
			};
			ast::Ast::BinOp(ast::BinOp {
				kind: ast::Type::None,
				op_ty,
				lhs: Box::new(to_lower_ast(&mut b.lhs)),
				rhs: Box::new(to_lower_ast(&mut b.rhs))
			})
		},
		AstNode::Application(a) => ast::Ast::Appl(ast::Appl {
			kind: ast::Type::None,
			left: Box::new(to_lower_ast(&mut a.lhs)),
			right: Box::new(to_lower_ast(&mut a.rhs)),
		}),
	}
}

/// Translate a higher level Type into a core Type
fn type_to_core_lang_type(t: &mut Type) -> ast::Type {
	// All of this is a direct, 1:1 lowering. Fairly intuitive & straightforward.
	match t {
		Type::Num => ast::Type::Num,
		Type::Unit => ast::Type::Unit,
		Type::Bool => ast::Type::Bool,
		Type::TypeVar(i) => ast::Type::TypeVar(ast::TypeVar {
			id: i.name.clone(),
			tok: i.token.clone().to_core_tok(),
		}),
		Type::AdtType(a) => ast::Type::Adt(ast::Adt {
			id: Box::new(ast::Ast::Ident(ast::Ident {
				kind: ast::Type::None,
				name: a.ident.name.clone(),
				token: a.ident.token.clone().to_core_tok(),
			})),
			args: a.args.iter_mut().map(|t| type_to_core_lang_type(t)).collect(),
		}),
		Type::ProdType(p) => ast::Type::Prod(ast::Prod {
			items: p.args.iter_mut().map(|t| type_to_core_lang_type(t)).collect(),
		}),
		Type::FnType(f) => ast::Type::Fn(ast::Fn {
			lhs: Box::new(type_to_core_lang_type(&mut f.lhs)),
			rhs: Box::new(type_to_core_lang_type(&mut f.rhs)),
		}),
	}
}
/// Given a high level lambda, which may define multiple arguments, extract the type
/// that the lambda would have if it were curried.
///
/// For example, given the lambda
/// ```
/// (a: Num, b: Bool): Num => ...
/// ```
/// we extract the type
/// ```
/// Num -> Bool -> Num
/// ```
fn extract_lambda_type(l: &mut Lambda) -> Type {
	// This is pretty easy, we just iterate through the arguments of the lambda and add their types to the final type.
	fn build_rec(args: &mut impl ExactSizeIterator<Item=Type>, ret_ty: Type) -> Type {
		if args.len() == 0 {
			ret_ty
		} else {
			Type::FnType(FnType {
				lhs: Box::new(args.next().expect("Iterator is empty (compiler error)")),
				rhs: Box::new(build_rec(args, ret_ty)),
			})
		}
	}

	build_rec(
		&mut l.args.iter().map(|(_, b)| b.clone()),
		l.ret_type.clone(),
	)
}

/// Translate a higher level pattern into a core pattern.
fn pattern_to_core_lang_pattern(p: &mut Pattern) -> ast::Pattern {
	// This is all a 1:1 translation.
	match p {
		Pattern::Any(i) => ast::Pattern::Any(ast::Ident {
			kind: ast::Type::None,
			name: i.name.clone(),
			token: i.token.clone().to_core_tok()
		}),
		Pattern::Adt { ident, args, pattern } => ast::Pattern::Adt(
			ast::Ident {
				kind: ast::Type::None,
				name: ident.name.clone(),
				token: ident.token.clone().to_core_tok()
			},
			args.iter_mut().map(|t| type_to_core_lang_type(t)).collect(),
			Box::new(pattern_to_core_lang_pattern(pattern))
		),
		Pattern::Num(n) => ast::Pattern::Num(ast::NumLit { val: *n, kind: ast::Type::Num }),
		Pattern::Bool(b) => ast::Pattern::Bool(ast::BoolLit { val: *b, kind: ast::Type::Bool }),
		Pattern::Unit => ast::Pattern::Unit,
		Pattern::Tuple(t) => ast::Pattern::Prod(
			t.iter_mut().map(|p| pattern_to_core_lang_pattern(p)).collect()
		)
	}
}