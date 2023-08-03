use crate::{ast::*, errors::*, lexer::*};


/*
	This code is not part of the compiler pipeline. This parser is an extremely simple,
	context-free, LL(1) parser for Core. However, the current pipeline goes straight
	from high level Ast to Core, with no parsing in-between. This code will stay
	only because it may be helpful in the future to parse Core.
*/

/// Hold the state needed for parsing.
/// This is very bare bones; just a list of tokens and an index counter.
struct Parser {
	pub toks: Vec<Token>,
	pub place: usize,
}

impl Parser {
	pub fn new(toks: Vec<Token>) -> Parser {
		Parser { toks, place: 0 }
	}

	pub fn peek(&mut self) -> Res<&Token> {
		match self.toks.get(self.place) {
			Some(t) => Ok(t),
			_ => Err(ErrMsg::new("Unexpected end of input")),
		}
	}

	pub fn accept_any(&mut self) -> Res<&Token> {
		match self.toks.get(self.place) {
			Some(t) => {
				self.place += 1;
				Ok(t)
			}
			None => Err(ErrMsg::new("Unexpected end of input")),
		}
	}

	pub fn accept_kind(&mut self, k: TokKind) -> Res<&Token> {
		let next = self.accept_any()?;
		if next.kind == k {
			Ok(next)
		} else {
			Err(
				ErrMsg::new(&format!("Expected {:?} but got {}", k, next.lexeme))
					.place(next.line, next.col),
			)
		}
	}
}

pub fn parse(toks: Vec<Token>) -> Res<Ast> {
	let mut parser = Parser::new(toks);
	let head = parse_expr(&mut parser)?;
	if let Ok(t) = parser.peek() {
		Err(ErrMsg::new(&format!("Could not parse {}", t.lexeme)).place(t.line, t.col))
	} else {
		Ok(head)
	}
}

/// Use the parser supplied to parse one or more times; stop on error, and return
/// a list of successful parses.
fn one_plus<T>(p: &mut Parser, f: fn(&mut Parser) -> Res<T>) -> Res<Vec<T>> {
	let mut results = vec![f(p)?];
	results.append(&mut zero_plus(p, f)?);
	Ok(results)
}

/// Use the parser supplied to parse zero or more times; stop on error, and
/// return a list of successful parses.
fn zero_plus<T>(p: &mut Parser, f: fn(&mut Parser) -> Res<T>) -> Res<Vec<T>> {
	let mut results = vec![];
	while let Ok(res) = f(p) {
		results.push(res);
	}
	Ok(results)
}

/// Use the parser supplied to parse one or more times. Only continue parsing
/// if the token matches the type supplied. Return a list of successful parses.
fn one_plus_checked<T>(p: &mut Parser, ty: TokKind, f: fn(&mut Parser) -> Res<T>) -> Res<Vec<T>> {
	let mut results = vec![f(p)?];
	results.append(&mut zero_plus_checked(p, ty, f)?);
	Ok(results)
}

/// Use the parser supplied to parse zero or more times. Only continue parsing
/// if the token matches the type supplied. Return a list of successful parses.
fn zero_plus_checked<T>(p: &mut Parser, ty: TokKind, f: fn(&mut Parser) -> Res<T>) -> Res<Vec<T>> {
	let mut results = vec![];
	while let Ok(next) = p.peek() {
		if next.kind != ty { break; }
		results.push(f(p)?);
	}
	Ok(results)
}

fn parse_expr(p: &mut Parser) -> Res<Ast> {
	let next = p.peek()?;
	let ast = match next.kind {
		TokKind::Appl => parse_appl(p),
		TokKind::TAppl => parse_tappl(p),
		TokKind::Abst => parse_abst(p),
		TokKind::TAbst => parse_tabst(p),
		TokKind::Ident => parse_ident(p),
		TokKind::If => parse_if(p),
		TokKind::Match => parse_match(p),
		TokKind::Let => parse_let(p),
		TokKind::Data => parse_data(p),
		TokKind::NumLit => parse_num_lit(p),
		TokKind::BoolLit => parse_bool_lit(p),
		TokKind::Prod => parse_prod_lit(p),
		TokKind::NAdd => parse_binop(p),
		TokKind::NSub => parse_binop(p),
		TokKind::NMul => parse_binop(p),
		TokKind::NDiv => parse_binop(p),
		TokKind::NMod => parse_binop(p),
		TokKind::NEq => parse_binop(p),
		TokKind::NNeq => parse_binop(p),
		TokKind::NGt => parse_binop(p),
		TokKind::NGte => parse_binop(p),
		TokKind::NLt => parse_binop(p),
		TokKind::NLte => parse_binop(p),
		TokKind::BEq => parse_binop(p),
		TokKind::BNeq => parse_binop(p),
		TokKind::BOr => parse_binop(p),
		TokKind::BAnd => parse_binop(p),
		_ => Err(
			ErrMsg::new(&format!("Expected expression but got {}", next.lexeme))
				.place(next.line, next.col),
		),
	}?;
	Ok(ast)
}

fn parse_appl(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::Appl)?;
	Ok(Ast::Appl(Appl {
		kind: Type::None,
		left: Box::new(parse_expr(p)?),
		right: Box::new(parse_expr(p)?),
	}))
}

fn parse_tappl(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::TAppl)?;
	Ok(Ast::TAppl(TAppl {
		kind: Type::None,
		left: Box::new(parse_expr(p)?),
		right: parse_type(p)?,
	}))
}

fn parse_abst(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::Abst)?;
	Ok(Ast::Abst(Abst {
		kind: Type::None,
		ident: Box::new(parse_ident(p)?),
		type_hint: parse_type(p)?,
		body: Box::new(parse_expr(p)?),
	}))
}

fn parse_tabst(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::TAbst)?;
	Ok(Ast::TAbst(TAbst {
		kind: Type::None,
		ident: parse_type(p)?,
		body: Box::new(parse_expr(p)?),
	}))
}

fn parse_ident(p: &mut Parser) -> Res<Ast> {
	let tok = p.accept_kind(TokKind::Ident)?;
	Ok(Ast::Ident(Ident {
		kind: Type::None,
		name: tok.lexeme.clone(),
		token: tok.clone(),
	}))
}

fn parse_if(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::If)?;
	Ok(Ast::If(If {
		kind: Type::None,
		condition: Box::new(parse_expr(p)?),
		t_stmt: Box::new(parse_expr(p)?),
		f_stmt: Box::new(parse_expr(p)?),
	}))
}

fn parse_match(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::Match)?;
	let lhs = parse_expr(p)?;
	Ok(Ast::Match(Match {
		kind: Type::None,
		lhs: Box::new(lhs),
		arms: zero_plus_checked(p, TokKind::Case, parse_match_arm)?
	}))
}

fn parse_match_arm(p: &mut Parser) -> Res<(Pattern, Option<Ast>, Ast)> {
	p.accept_kind(TokKind::Case)?;
	let pattern = parse_pattern(p)?;

	let guard = if p.peek()?.kind != TokKind::To {
		Some(parse_expr(p)?)
	} else { None };

	p.accept_kind(TokKind::To)?;
	let rhs = parse_expr(p)?;

	Ok((pattern, guard, rhs))
}

fn parse_pattern(p: &mut Parser) -> Res<Pattern> {
	let next = p.peek()?;
	match &next.kind {
		TokKind::Ident => {
			Ok(Pattern::Any(cast!(parse_ident(p)?, Ast::Ident)))
		}
		TokKind::Adt => {
			p.accept_any()?;
			let id = cast!(parse_ident(p)?, Ast::Ident);
			Ok(Pattern::Adt(id, zero_plus_checked(p, TokKind::At, parse_type)?, Box::new(parse_pattern(p)?)))
		}
		TokKind::NumLit => {
			Ok(Pattern::Num(cast!(parse_num_lit(p)?, Ast::NumLit)))
		}
		TokKind::BoolLit => {
			Ok(Pattern::Bool(cast!(parse_bool_lit(p)?, Ast::BoolLit)))
		}
		TokKind::Prod => {
			p.accept_any()?;
			p.accept_kind(TokKind::LParen)?;
			let patterns = one_plus(p, parse_pattern)?;
			p.accept_kind(TokKind::RParen)?;
			Ok(Pattern::Prod(patterns))
		}
		_ => Err(ErrMsg::new(&format!("Expected pattern but got {}", next.lexeme)))
	}
}

fn parse_let(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::Let)?;
	Ok(Ast::Let(Let {
		kind: Type::None,
		id: Box::new(parse_ident(p)?),
		type_hint: parse_type(p)?,
		lhs: Box::new(parse_expr(p)?),
		rhs: Box::new(parse_expr(p)?),
	}))
}

fn parse_data(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::Data)?;
	Ok(Ast::Data(Data {
		id: Box::new(parse_ident(p)?),
		kind: Type::None,
		args: zero_plus_checked(p, TokKind::At, parse_data_arg)?,
		constructors: zero_plus_checked(p, TokKind::Ident, parse_data_constructor)?,
		rhs: Some(Box::new(parse_expr(p)?)),
	}))
}

fn parse_data_arg(p: &mut Parser) -> Res<Type> {
	p.accept_kind(TokKind::At)?;
	let tok = p.accept_kind(TokKind::Ident)?;
	Ok(Type::TypeVar(TypeVar {
		tok: tok.clone(),
		id: tok.lexeme.clone(),
	}))
}

fn parse_data_constructor(p: &mut Parser) -> Res<Ast> {
	Ok(Ast::Constructor(Constructor {
		id: Box::new(parse_ident(p)?),
		data_type: parse_type(p)?,
		kind: Type::None,
		tag: 0,
	}))
}

fn parse_num_lit(p: &mut Parser) -> Res<Ast> {
	let tok = p.accept_kind(TokKind::NumLit)?;
	Ok(Ast::NumLit(NumLit {
		kind: Type::Num,
		val: tok
			.lexeme
			.parse::<f64>()
			.expect("Could not parse str->float (this is a compiler error)"),
	}))
}

fn parse_bool_lit(p: &mut Parser) -> Res<Ast> {
	let tok = p.accept_kind(TokKind::BoolLit)?;
	Ok(Ast::BoolLit(BoolLit {
		kind: Type::Bool,
		val: tok
			.lexeme
			.parse::<bool>()
			.expect("Could not parse str->bool (this is a compiler error)"),
	}))
}

fn parse_prod_lit(p: &mut Parser) -> Res<Ast> {
	p.accept_kind(TokKind::Prod)?;
	p.accept_kind(TokKind::LParen)?;
	let prod = Ast::ProdLit(ProdLit {
		items: one_plus(p, parse_expr)?,
	});
	p.accept_kind(TokKind::RParen)?;
	Ok(prod)
}

fn parse_type(p: &mut Parser) -> Res<Type> {
	p.accept_kind(TokKind::At)?;
	let next = p.peek()?.clone();
	match next.kind {
		TokKind::BoolTy => {
			p.accept_any()?;
			Ok(Type::Bool)
		}
		TokKind::NumTy => {
			p.accept_any()?;
			Ok(Type::Num)
		}
		TokKind::Ident => parse_type_var(p),
		TokKind::Fn => {
			p.accept_any()?;
			Ok(Type::Fn(Fn {
				lhs: Box::new(parse_type(p)?),
				rhs: Box::new(parse_type(p)?),
			}))
		}
		TokKind::Adt => {
			p.accept_any()?;
			p.accept_kind(TokKind::LParen)?;
			let adt = Type::Adt(Adt {
				id: Box::new(parse_ident(p)?),
				args: zero_plus_checked(p, TokKind::At, parse_type)?,
			});
			p.accept_kind(TokKind::RParen)?;
			Ok(adt)
		}
		TokKind::Prod => {
			p.accept_any()?;
			p.accept_kind(TokKind::LParen)?;
			let prod = Type::Prod(Prod {
				items: one_plus_checked(p, TokKind::At, parse_type)?,
			});
			p.accept_kind(TokKind::RParen)?;
			Ok(prod)
		}
		TokKind::Forall => {
			p.accept_any()?;
			Ok(Type::Universal(Universal {
				arg: Box::new(parse_type_var(p)?),
				body: Box::new(parse_type(p)?),
			}))
		}
		_ => {
			Err(ErrMsg::new(&format!("Expected type, got {}", next.lexeme))
				.place(next.line, next.col))
		}
	}
}

fn parse_type_var(p: &mut Parser) -> Res<Type> {
	let tok = p.accept_kind(TokKind::Ident)?;
	Ok(Type::TypeVar(TypeVar {
		tok: tok.clone(),
		id: tok.lexeme.clone(),
	}))
}


fn parse_binop(p: &mut Parser) -> Res<Ast> {
	let next = p.peek()?.clone();
	let ty = match next.kind {
		TokKind::NAdd => OpTy::NumAdd,
		TokKind::NSub => OpTy::NumSub,
		TokKind::NMul => OpTy::NumMul,
		TokKind::NDiv => OpTy::NumDiv,
		TokKind::NMod => OpTy::NumMod,
		TokKind::NNeq => OpTy::NumNeq,
		TokKind::NEq => OpTy::NumEq,
		TokKind::NGt => OpTy::NumGt,
		TokKind::NGte => OpTy::NumGte,
		TokKind::NLt => OpTy::NumLt,
		TokKind::NLte => OpTy::NumLte,
		TokKind::BEq => OpTy::BoolEq,
		TokKind::BNeq => OpTy::BoolNeq,
		TokKind::BOr => OpTy::BoolOr,
		TokKind::BAnd => OpTy::BoolAnd,
		_ => return Err(ErrMsg::new(&format!("Expected operator, got {}", next.lexeme)))
	};

	p.accept_any()?;

	Ok(Ast::BinOp(BinOp {
		kind: Type::None,
		op_ty: ty,
		lhs: Box::new(parse_expr(p)?),
		rhs: Box::new(parse_expr(p)?)
	}))
}