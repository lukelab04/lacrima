pub mod scopes;

use crate::ast::*;
use crate::lexer::{TokKind, Token};
use core_lang::errors::*;
use crate::namespace::{NameEntry, NameSpacePath};
use crate::parser::scopes::ScopeManager;

/*
	Parsing is a little tricky. If you take a look at grammar.txt, you'll notice that the syntax is context-dependant.
	The way we handle this in a recursive descent, non-backtracking parser is by keeping track of the types of identifiers.
	For example, in the 'Pattern' grammar rule, we see that an Adt pattern is 'Ident (Type*) Pattern'. Clearly, this
	causes issues -- both 'Type' and 'Pattern' can start with an identifier, so we have no idea when the types stop
	and the pattern begins. However, by looking up the Adt identifier, we can query the number of type arguments
	it takes, and then parse exactly that many types. Thus, we avoid backtracking.

	We parse expressions with a Pratt parser. This is relatively straightforward, and can be easily embedded within
	the recursive descent parse.r

	The last tricky bit here is name resolution. Ideally, this would be done in a separate step.
	However, name resolution requires a parsed input, and parsing requires name resolution. Most of the
	logic regarding name resolution is wrapped up in the ScopeManager struct -- all we really
	need to do here is declare scopes, variables, and the like.
*/

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[allow(unused)]
/// Keep track of precedence levels for tokens
pub enum Precedence {
	None = -1,
	Composition = 0,
	Logical = 4,
	Equality = 5,
	Comparison = 6,
	Sum = 10,
	Product = 20,
	Prefix = 30,
	Postfix = 40,
	Application = 50,
}

/// Keep track of the current parsing state.
struct Parser {
	tokens: Vec<Token>,
	place: usize,
	sm: ScopeManager,
}

impl Parser {
	pub fn new(tokens: Vec<Token>, sm: ScopeManager) -> Parser {
		Parser {
			sm,
			tokens,
			place: 0,
		}
	}
	/// We are currently on a `'('` character. Is this the start of a tuple?
	///
	/// Internally, we just check for the existence of commas between elements.
	/// If there are commas, we have a tuple. If not, no tuple.
	/// # Panics
	/// Panics if called on a token that is not `'('`.
	pub fn is_tuple(&self) -> bool {
		if self.place >= self.tokens.len() {
			return false;
		}
		if self.tokens[self.place].lexeme != "(" {
			panic!("'is_tuple' must be called while parser is on RParen (compiler error)")
		}
		// Note that lambdas may also contain inner commas. So, called on a lambda,
		// this function would return true. However, the is_lambda function cannot mistake
		// a tuple for a lambda, so we perform that check here.
		if self.is_lambda() { return false; }
		let mut idx = self.place;
		// We must keep track of the current nesting level. For example, the expression
		// ((a, b)) *contains* a tuple, but is not *itself* a tuple, despite the existence
		// of inner commas. So we only want to note a comma if it occurs within the outermost
		// nesting level.
		let mut paren_nesting = 0;
		while idx < self.tokens.len() {
			if paren_nesting == 1 && self.tokens[idx].lexeme == "," {
				return true;
			}
			if self.tokens[idx].lexeme == "(" {
				paren_nesting += 1;
			}
			if self.tokens[idx].lexeme == ")" && paren_nesting > 0 {
				paren_nesting -= 1;
			}
			if self.tokens[idx].lexeme == ")" && paren_nesting == 0 {
				break;
			}
			idx += 1;
		}

		return false;
	}
	/// We are currently on a `'('` character. Is this the start of a lambda?
	///
	/// Internally, we just skip to the closing `')'`, then check for the existence
	/// of a type signature, which is required for lambdas. If yes, lambda. If no, no lambda.
	/// # Panics
	/// Panics if called on a non `'('` character.
	pub fn is_lambda(&self) -> bool {
		if self.place >= self.tokens.len() {
			return false;
		}
		if self.tokens[self.place].lexeme != "(" {
			panic!("'is_lambda' must be called while parser is on RParen (compiler error)")
		}
		let mut idx = self.place;
		// We keep track of parenthesis nesting so we know when we have exited the expression.
		let mut paren_nesting = 0;
		while idx < self.tokens.len() {
			if self.tokens[idx].lexeme == "(" {
				paren_nesting += 1;
			}
			if self.tokens[idx].lexeme == ")" {
				paren_nesting -= 1;
			}
			if self.tokens[idx].lexeme == ")" && paren_nesting == 0 {
				break;
			}
			idx += 1;
		}
		// Index is within bounds, and the next character starts a type hint
		(idx + 1) < self.tokens.len() && self.tokens[idx + 1].lexeme == ":"
	}
	/// We are on the first token after the data identifier. How many type arguments does this data type take?
	pub fn count_data_args(&self) -> usize {
		let mut i = self.place;
		let mut count = 0;

		loop {
			// Should have been caught earlier
			if i >= self.tokens.len() { panic!("Hit end of input while getting data args (compiler error)") }
			// Bars start the constructor section of a data declaration
			if self.tokens[i].kind == TokKind::Bar { break; }
			if self.tokens[i].kind == TokKind::Ident { count += 1; }
			i += 1;
		}

		count
	}
	/// Map from a token type to an expression precedence level.
	/// Type operators, like '->', have a precedence, but will return `Precedence::None`
	/// here, because within the context of an expression they have no precedence.
	pub fn get_expr_precedence(&self) -> Precedence {
		if self.place >= self.tokens.len() {
			Precedence::None
		} else {
			match &self.tokens[self.place].kind {
				TokKind::Add | TokKind::Sub => Precedence::Sum,
				TokKind::Mul | TokKind::Div | TokKind::Mod => Precedence::Product,
				TokKind::CmpEq | TokKind::CmpNeq => Precedence::Equality,
				TokKind::Gt | TokKind::Gte | TokKind::Lt | TokKind::Lte => Precedence::Comparison,
				TokKind::And | TokKind::Or => Precedence::Logical,

				TokKind::Ident
				| TokKind::If
				| TokKind::Match
				| TokKind::Let
				| TokKind::BoolLit
				| TokKind::NumLit
				| TokKind::LParen => Precedence::Application,

				_ => Precedence::None,
			}
		}
	}
	/// Map from a token type to a type precedence level.
	pub fn get_type_precedence(&self) -> Precedence {
		if self.place >= self.tokens.len() {
			return Precedence::None;
		}
		match &self.tokens[self.place].kind {
			TokKind::ThinArrow => Precedence::Composition,
			_ => Precedence::None,
		}
	}
	/// Accept a token of a specific type.
	/// # Errors
	/// Return an error if the next token is not of the specified type, or if
	/// there are no more tokens.
	pub fn accept_type(&mut self, ty: TokKind) -> Res<Token> {
		if self.place >= self.tokens.len() {
			Err(ErrMsg::new("Unexpected end of input"))
		} else {
			let tmp = self.tokens[self.place].clone();
			if tmp.kind != ty {
				return Err(ErrMsg::new(&format!(
					"Expected {:?} but got {:?}",
					ty, tmp.kind
				)));
			}
			self.place += 1;
			Ok(tmp)
		}
	}
	/// Accept any token.
	/// # Errors
	/// Return an error if there are no more tokens.
	pub fn accept_any(&mut self) -> Res<Token> {
		if self.place >= self.tokens.len() {
			Err(ErrMsg::new("Unexpected end of input"))
		} else {
			let tmp = self.tokens[self.place].clone();
			self.place += 1;
			Ok(tmp)
		}
	}
	/// Peek the next token without advancing the internal index marker.
	/// # Errors
	/// Return an error if there are no more tokens
	pub fn peek(&self) -> Res<&Token> {
		self.tokens
			.get(self.place)
			.ok_or(ErrMsg::new("Unexpected end of input"))
	}
}

/// Given a list of tokens and a scope, parse into an AstNode.
/// # Errors
/// * Return an error if the list of tokens is empty.
/// * Return an error for any parse errors.
pub fn parse(toks: Vec<Token>, scope_manager: ScopeManager) -> Res<AstNode> {
	let mut parser = Parser::new(toks, scope_manager);

	// The 'Module' node that represents files takes a list of inner Ast nodes.
	let mut nodes = vec![];

	// The only top level nodes are Fn, Data, Import, Export, and Extern.
	while let Ok(t) = parser.peek() {
		match t.kind {
			TokKind::Fn => nodes.push(parse_named_fn(&mut parser, false)?),
			TokKind::Data => nodes.push(parse_data(&mut parser, false)?),
			TokKind::Import => {
				// We don't actually need to construct any nodes here.
				// The namespace resolver did the heavy lifting for us.
				parser.accept_any()?;
				parse_namespace(&mut parser)?;
			}
			TokKind::Export => {
				// Although we already handled exporting names, we still need to parse
				// the bodies of the exported nodes.
				parser.accept_any()?;
				match parser.peek()?.kind {
					TokKind::Fn => nodes.push(parse_named_fn(&mut parser, true)?),
					TokKind::Data => nodes.push(parse_data(&mut parser, true)?),
					_ => panic!("Unhandled export (compiler error)")
				}
			}
			TokKind::Extern => nodes.push(parse_extern(&mut parser)?),
			_ => {
				return Err(ErrMsg::new(&format!(
					"Expected 'data', 'fn', 'import', 'export', or 'extern', but got '{}'",
					t.lexeme
				)))
			}
		}
	}

	if nodes.is_empty() {
		return Err(ErrMsg::new("File is empty"));
	}

	Ok(AstNode::Module(nodes))
}

// 'fn' Ident Lambda
fn parse_named_fn(p: &mut Parser, exported: bool) -> Res<AstNode> {
	let tok = p.accept_type(TokKind::Fn)?;
	let mut id = parse_ident(p)?;
	let orig_name = id.as_ident_immut().name.clone();

	let mut qualified_name = None;

	// If already exported, we don't need to newly register the name.
	if !exported {
		p.sm.register_standard_ident(id.as_ident_mut())?;
	} else {
		// Even with exported names, we need to make sure they are registered *within this scope* -- something
		// that the namespace resolver cannot do.
		p.sm.register_exported_ident(id.as_ident_mut())?;
		let mut tmp = p.sm.get_namespace_path();
		tmp.append(&orig_name);
		qualified_name = Some(tmp);
	}

	// Functions are scoping
	p.sm.enter_scope();
	let lam = parse_lambda(p)?;
	p.sm.exit_scope();

	Ok(AstNode::NamedFn(NamedFn {
		qualified_name,
		token: tok,
		id: Box::new(id),
		lambda: Box::new(lam),
	}))
}

// '(' (Ident TypeHint ,?)+ ')' ':' Type '=>' Expression
fn parse_lambda(p: &mut Parser) -> Res<AstNode> {
	p.accept_type(TokKind::LParen)?;
	// Lambdas are scoping
	p.sm.enter_scope();
	let mut args = vec![];
	let mut next = p.peek()?;

	// Parse all the arguments, ignoring commas
	while next.kind != TokKind::RParen {
		let mut arg = parse_ident(p)?;
		p.sm.register_standard_ident(arg.as_ident_mut())?;
		let hint = parse_type_hint(p)?;
		args.push((arg, hint));
		next = p.peek()?;
		if next.kind == TokKind::Comma {
			p.accept_any()?;
			next = p.peek()?;
		}
	}

	if args.is_empty() {
		return Err(ErrMsg::new("Function must specify at least 1 argument"))
	}

	// Get the type
	p.accept_type(TokKind::RParen)?;
	p.accept_type(TokKind::Colon)?;
	let return_type = parse_type(p, Precedence::None)?;
	p.accept_type(TokKind::ThickArrow)?;

	// Get the body
	let body = parse_expression(p, Precedence::None)?;

	p.sm.exit_scope();

	Ok(AstNode::Lambda(Lambda {
		args,
		ret_type: return_type,
		body: Box::new(body),
	}))
}

// Extern Ident TypeHint
fn parse_extern(p: &mut Parser) -> Res<AstNode> {
	let token = p.accept_type(TokKind::Extern)?;
	let mut id = parse_ident(p)?;
	// Make sure others can see this identifier
	p.sm.register_extern_ident(id.as_ident_mut())?;
	Ok(AstNode::Extern(Extern {
		token,
		id: Box::new(id),
		ty: parse_type_hint(p)?,
	}))
}

// Data Ident Ident* ('|' Ident Type)+
fn parse_data(p: &mut Parser, exported: bool) -> Res<AstNode> {
	let token = p.accept_type(TokKind::Data)?;
	let mut data_id = parse_ident(p)?;

	// With non-exported data types, we need to declare their names.
	// Otherwise, we just make them visible within this scope.
	if !exported {
		p.sm.register_data(data_id.as_ident_mut(), p.count_data_args())?;
	} else {
		p.sm.register_exported_ident(data_id.as_ident_mut())?;
	}

	// We must scope to avoid interference with the type variable declarations.
	p.sm.enter_scope();

	// Collect type args
	let mut args = vec![];
	let mut next = p.peek()?;
	while next.kind != TokKind::Bar {
		let mut id = parse_ident(p)?;
		p.sm.register_type_var(id.as_ident_mut())?;
		args.push(id);
		next = p.peek()?;
	}


	// Gotta reinitialize it so the borrow checker doesn't complain
	let mut next = p.peek()?;
	let mut cons = vec![];
	// Constructors should be declared in the same scope as the data identifier.
	// However, we can't pop scope here, or else we would lose access to the type
	// variables declared above. So, we just keep track of the constructor
	// identifiers we see, then declare them later after we can pop scope.
	let mut cons_ids = vec![];

	while next.kind == TokKind::Bar {
		p.accept_any()?;
		let cons_id = parse_ident(p)?;
		cons_ids.push(cons_id);

		let ty = parse_type(p, Precedence::None)?;
		cons.push(ty);
		if let Ok(n) = p.peek() {
			next = n;
		} else {
			break;
		}
	}

	p.sm.exit_scope();

	// Register all those constructors we collected above
	for c in &mut cons_ids {
		// Exported data items already declared their constructors
		if !exported {
			let orig_name = c.as_ident_immut().name.clone();
			p.sm.register_cons(c.as_ident_mut(), args.len())?;
			p.sm.add_cons_to_data(&data_id.as_ident_immut().name, &orig_name);
		} else {
			p.sm.register_exported_ident(c.as_ident_mut())?;
		}
	}

	// Vec<AstNode>, Vec<Type> => Vec<(AstNode, Type)>
	// This is the format the Data struct expects
	let cons = cons_ids.into_iter().zip(cons.into_iter()).collect();

	Ok(AstNode::Data(Data {
		token,
		id: Box::new(data_id),
		args,
		constructors: cons,
	}))
}

// ':' Type
fn parse_type_hint(p: &mut Parser) -> Res<Type> {
	p.accept_type(TokKind::Colon)?;
	parse_type(p, Precedence::None)
}

// Atomic, Adt, Product, Fn, TypeVar, Group
fn parse_type(p: &mut Parser, prec: Precedence) -> Res<Type> {
	// This is where we move into Pratt parsing. The first step is pretty straightforward -- just parse a type.
	let mut next = p.peek()?;
	let mut ty = match next.kind {
		TokKind::NumType => parse_num_type(p)?,
		TokKind::BoolType => parse_bool_type(p)?,
		// Left parens could mean a few different things, depending on context.
		TokKind::LParen => {
			if p.is_tuple() {
				parse_tuple_type(p)?
			} else if p.place < p.tokens.len() - 1 && p.tokens[p.place + 1].kind == TokKind::RParen {
				parse_unit_type(p)?
			} else {
				p.accept_any()?;
				let t = parse_type(p, Precedence::None)?;
				p.accept_type(TokKind::RParen)?;
				t
			}
		}
		TokKind::Ident => {
			#[allow(unused)]
				let mut debug_name = String::new();

			// We are allowed to do things like 'other::namespace::DataType'.
			// These cases are handled differently.
			let id = parse_ident_or_namespace(p)?;
			let id_ty = if let Some(mut left) = id.0 {
				// In the case of a regular identifier, we just look it up in the resolver.
				// If it exists, it must be an AdtType, because regular idents and constructors are not types.
				// If it does not exist, we must be declaring a new type variable.
				debug_name = left.as_ident_immut().name.clone();
				match p.sm.resolve_local(left.as_ident_mut()) {
					Ok(NameEntry::Standard { .. }) | Ok(NameEntry::Constructor { .. }) => return Err(ErrMsg::new(&format!("Expected type variable or Adt, got {}", left.as_ident_immut().name))),
					Ok(e) => e,
					Err(_) => {
						p.sm.register_type_var(left.as_ident_mut())?;
						NameEntry::TypeVar { id: left.as_ident_mut().clone() }
					}
				}
			} else if let Some(right) = id.1 {
				// In the case of a namespace, we just need to resolve the namespace.
				// Everything else is handled for us.
				debug_name = right.to_str();
				match p.sm.resolve_namespace(&right) {
					Ok(e) => e,
					Err(_) => {
						return Err(ErrMsg::new(&format!("Name {} does not exist", right.to_str())))
					}
				}
			} else {
				panic!("Both sides of 'parse_ident_or_namespace' are None (compiler error)")
			};

			// Convert the two options we had for identifiers (Data types of type variables)
			// into corresponding Ast nodes.
			match id_ty {
				NameEntry::TypeVar { id } => Type::TypeVar(id),
				NameEntry::Data { arguments, id, .. } => {
					let mut args = vec![];
					// A specified number of type arguments must be present.
					for _ in 0..arguments {
						args.push(parse_type(p, Precedence::Application)?);
					}
					Type::AdtType(AdtType {
						ident: id,
						args,
					})
				}
				_ => {
					return Err(ErrMsg::new(&format!(
						"Expected type variable or Adt, got '{}'",
						debug_name
					)))
				}
			}
		}
		_ => {
			return Err(ErrMsg::new(&format!(
				"Expected type, got '{}'",
				next.lexeme
			)))
		}
	};

	// Here is where the Pratt parsing comes in. We check the next token
	// for any potential type operators, and work from there.

	while prec < p.get_type_precedence() {
		next = if let Ok(n) = p.peek() {
			n
		} else {
			break;
		};
		ty = match next.kind {
			TokKind::ThinArrow => parse_fn_type(p, ty)?,
			_ => {
				return Err(ErrMsg::new(&format!(
					"Expected type but got '{}'",
					next.lexeme
				)))
			}
		};
	}

	Ok(ty)
}

// Num
fn parse_num_type(p: &mut Parser) -> Res<Type> {
	p.accept_type(TokKind::NumType)?;
	Ok(Type::Num)
}

// ()
fn parse_unit_type(p: &mut Parser) -> Res<Type> {
	p.accept_type(TokKind::LParen)?;
	p.accept_type(TokKind::RParen)?;
	Ok(Type::Unit)
}

// Bool
fn parse_bool_type(p: &mut Parser) -> Res<Type> {
	p.accept_type(TokKind::BoolType)?;
	Ok(Type::Bool)
}

// Type -> Type
fn parse_fn_type(p: &mut Parser, left: Type) -> Res<Type> {
	p.accept_type(TokKind::ThinArrow)?;
	Ok(Type::FnType(FnType {
		lhs: Box::new(left),
		// Parse with prec none to make right associative
		rhs: Box::new(parse_type(p, Precedence::None)?),
	}))
}

// '(' Type+ ,? ')'
fn parse_tuple_type(p: &mut Parser) -> Res<Type> {
	p.accept_type(TokKind::LParen)?;
	let mut next = p.peek()?;
	let mut args = vec![];
	// Accept everything within the parenthesis, ignoring commas
	while next.kind != TokKind::RParen {
		args.push(parse_type(p, Precedence::None)?);
		next = p.peek()?;
		if next.kind != TokKind::Comma {
			break;
		} else {
			p.accept_any()?;
			next = p.peek()?;
		}
	}
	p.accept_type(TokKind::RParen)?;
	Ok(Type::ProdType(ProdType { args }))
}

// Let, If, Match, Literal, Ident, Operation
fn parse_expression(p: &mut Parser, prec: Precedence) -> Res<AstNode> {
	// Here we switch from regular recursive descent into Pratt parsing. The first step
	// is pretty straightforward -- we just parse a single 'unit' of expression.
	let mut next = p.peek()?;
	let mut expr = match next.kind {
		TokKind::Let => parse_let(p)?,
		TokKind::If => parse_if(p)?,
		TokKind::Match => parse_match(p)?,
		TokKind::NumLit => parse_num(p)?,
		TokKind::BoolLit => parse_bool(p)?,
		TokKind::Ident => {
			// We allow things like 'other::namespace::x', which is resolved differently
			// than an in-scope variable. We handle that difference here.
			let id = parse_ident_or_namespace(p)?;
			if let Some(mut id) = id.0 {
				// Regular identifiers can be immediately resolved
				p.sm.resolve_local(id.as_ident_mut())?;
				id
			} else if let Some(ns) = id.1 {
				// Namespaced identifiers must point to either a constructor or a regular identifier.
				// Data types and type variables are not legal here.
				match p.sm.resolve_namespace(&ns)? {
					NameEntry::Constructor { id, .. } => AstNode::Ident(id),
					NameEntry::Standard { id, .. } => AstNode::Ident(id),
					_ => return Err(ErrMsg::new("Expected constructor or identifier"))
				}
			} else {
				panic!("'parse_ident_or_namespace' is empty on both sides (compiler error) ")
			}
		}
		TokKind::LParen => {
			if p.is_tuple() {
				parse_tuple(p)?
			} else if p.is_lambda() {
				parse_lambda(p)?
			} else if p.place < p.tokens.len() - 1 && p.tokens[p.place + 1].kind == TokKind::RParen {
				parse_unit_lit(p)?
			} else {
				parse_group(p)?
			}
		}
		_ => {
			return Err(ErrMsg::new(&format!(
				"Expected expression, got '{}'",
				next.lexeme
			)))
		}
	};

	// Here is where the Pratt parser comes in. Basically, as long as the next token
	// could be part of this expression and has a compatible precedence level, we accept it.
	while prec < p.get_expr_precedence() {
		next = p.peek()?;
		expr = match next.kind {
			// If we run across anything that isn't an operator, then we must be parsing an application.
			TokKind::Let
			| TokKind::LParen
			| TokKind::If
			| TokKind::Match
			| TokKind::NumLit
			| TokKind::BoolLit
			| TokKind::Ident => parse_application(p, expr)?,
			TokKind::Add
			| TokKind::Sub
			| TokKind::Mul
			| TokKind::Div
			| TokKind::Mod
			| TokKind::CmpEq
			| TokKind::CmpNeq
			| TokKind::And
			| TokKind::Gt
			| TokKind::Gte
			| TokKind::Lt
			| TokKind::Lte
			| TokKind::Or => parse_binary_operator(p, expr)?,
			_ => {
				return Err(ErrMsg::new(&format!(
					"Expected expression or operator, got '{}'",
					next.lexeme
				)))
			}
		}
	}

	Ok(expr)
}

// 'let' Ident TypeHint '=' Expression 'in' Expression
fn parse_let(p: &mut Parser) -> Res<AstNode> {
	// We first need to register this identifier.
	let token = p.accept_type(TokKind::Let)?;
	let mut ident = parse_ident(p)?;
	p.sm.register_standard_ident(ident.as_ident_mut())?;
	let hint = parse_type_hint(p)?;
	p.accept_type(TokKind::Equals)?;

	// Let is scoped.
	p.sm.enter_scope();
	let lhs = parse_expression(p, Precedence::None)?;
	p.sm.exit_scope();

	// We need another scope for the second expression in the let block.
	p.accept_type(TokKind::In)?;
	p.sm.enter_scope();
	let rhs = parse_expression(p, Precedence::None)?;
	p.sm.exit_scope();

	Ok(AstNode::Let(Let {
		token,
		hint,
		id: Box::new(ident),
		lhs: Box::new(lhs),
		rhs: Box::new(rhs),
	}))
}

// 'if' Expression 'then' Expression 'else' Expression
fn parse_if(p: &mut Parser) -> Res<AstNode> {
	let tok = p.accept_type(TokKind::If)?;
	let condition = parse_expression(p, Precedence::None)?;
	p.accept_type(TokKind::Then)?;

	// The true block is scoped
	p.sm.enter_scope();
	let lhs = parse_expression(p, Precedence::None)?;
	p.sm.exit_scope();
	p.accept_type(TokKind::Else)?;

	// The false block is scoped
	p.sm.enter_scope();
	let rhs = parse_expression(p, Precedence::None)?;
	p.sm.exit_scope();

	Ok(AstNode::If(If {
		token: tok,
		condition: Box::new(condition),
		t_stmt: Box::new(lhs),
		f_stmt: Box::new(rhs),
	}))
}

// 'match' Expression ('|' Pattern ('if' Expression)? '=>' Expression)+
fn parse_match(p: &mut Parser) -> Res<AstNode> {
	let token = p.accept_type(TokKind::Match)?;
	let expr = parse_expression(p, Precedence::None)?;

	// Each arm of the match block starts with a '|'.
	// Once we run out of '|', we know the match has ended.
	let mut next = p.peek()?;
	let mut arms = vec![];
	while next.kind == TokKind::Bar {
		// The arms are scoped.
		p.sm.enter_scope();

		p.accept_any()?;
		let pattern = parse_pattern(p)?;

		// Accept the guard, if one exists
		next = p.peek()?;
		let guard = if next.kind == TokKind::If {
			p.accept_any()?;
			Some(parse_expression(p, Precedence::None)?)
		} else {
			None
		};

		// Accept the expression
		p.accept_type(TokKind::ThickArrow)?;
		let body = parse_expression(p, Precedence::None)?;

		p.sm.exit_scope();

		arms.push((pattern, guard, body));
		next = if let Ok(n) = p.peek() {
			n
		} else {
			break;
		};
	}

	Ok(AstNode::Match(Match {
		token,
		expr: Box::new(expr),
		arms,
	}))
}

// Wildcard, Adt, Num, Bool, Unit, Tuple
fn parse_pattern(p: &mut Parser) -> Res<Pattern> {
	let next = p.peek()?;
	match next.kind {
		// An identifier could either be a wildcard pattern or the start of an Adt pattern.
		TokKind::Ident => {
			let id = parse_ident_or_namespace(p)?;
			let resolved;

			let is_constructor;
			let mut args = 0;

			// The identifier is not namespaced. It could either be the start of an Adt pattern, or a wildcard.
			if let Some(mut left) = id.0 {
				// We check if it is a constructor, which starts Adt patterns. If so, resolve it as a constructor.
				// Otherwise, resolve it as a wildcard.
				if let Some(a) = p.sm.is_constructor(&left.as_ident_immut().name) {
					args = a;
					is_constructor = true;
					p.sm.resolve_local(left.as_ident_mut())?;
				} else {
					is_constructor = false;
					p.sm.register_standard_ident(left.as_ident_mut())?;
				}
				resolved = left.to_ident();
			}

			// The identifier is namespaced. Thus, it cannot be a wildcard, so we resolve it as an Adt.
			else if let Some(right) = id.1 {
				is_constructor = true;
				resolved = match p.sm.resolve_namespace(&right)? {
					NameEntry::Constructor { id, .. } => id,
					_ => panic!("Expected {} to be a constructor (compiler error)", right.to_str()),
				}
			} else {
				panic!("Both sides of 'parse_ident_or_namespace' are None (compiler error)")
			}

			// Depending on whether we have a Adt or a wildcard, we construct an Ast node.
			if is_constructor {
				let mut type_args = vec![];
				for _ in 0..args {
					type_args.push(parse_type(p, Precedence::None)?);
				}
				let pattern = parse_pattern(p)?;
				Ok(Pattern::Adt {
					ident: resolved,
					args: type_args,
					pattern: Box::new(pattern)
				})
			} else {
				Ok(Pattern::Any(resolved))
			}
		}
		TokKind::NumLit => {
			let num = p
				.accept_any()?
				.lexeme
				.parse::<f64>()
				.expect("Could not parse number (compiler error)");
			Ok(Pattern::Num(num))
		}
		TokKind::BoolLit => {
			let b = p
				.accept_any()?
				.lexeme
				.parse::<bool>()
				.expect("Could not parse bool (compiler error)");
			Ok(Pattern::Bool(b))
		}
		// Left parens could be a tuple or a unit.
		TokKind::LParen => {
			if p.place < p.tokens.len() - 1 && p.tokens[p.place + 1].kind == TokKind::RParen {
				p.accept_any()?;
				p.accept_any()?;
				Ok(Pattern::Unit)
			} else {
				p.accept_any()?;
				let mut next = p.peek()?;
				let mut args = vec![];
				while next.kind != TokKind::RParen {
					args.push(parse_pattern(p)?);
					next = p.peek()?;
					if next.kind != TokKind::Comma {
						break;
					}
					p.accept_any()?;
					next = p.peek()?;
				}
				p.accept_type(TokKind::RParen)?;
				Ok(Pattern::Tuple(args))
			}
		}
		_ => Err(ErrMsg::new(&format!(
			"Expected pattern, got '{}'",
			next.lexeme
		))),
	}
}

// '(' Expression ')'
fn parse_group(p: &mut Parser) -> Res<AstNode> {
	p.accept_type(TokKind::LParen)?;
	let ast = parse_expression(p, Precedence::None)?;
	p.accept_type(TokKind::RParen)?;
	Ok(ast)
}

// '(' Expression+ ,? ')'
fn parse_tuple(p: &mut Parser) -> Res<AstNode> {
	p.accept_type(TokKind::LParen)?;
	let mut next = p.peek()?;
	let mut args = vec![];
	while next.kind != TokKind::RParen {
		args.push(parse_expression(p, Precedence::None)?);
		next = p.peek()?;
		if next.kind == TokKind::Comma {
			p.accept_any()?;
			next = p.peek()?;
		} else {
			break;
		}
	}
	p.accept_type(TokKind::RParen)?;
	Ok(AstNode::Tuple(args))
}

// Single number
fn parse_num(p: &mut Parser) -> Res<AstNode> {
	let tok = p.accept_type(TokKind::NumLit)?;
	Ok(AstNode::NumLit(
		tok.lexeme
			.parse::<f64>()
			.expect("Could not parse number (compiler error)"),
	))
}

// true | false
fn parse_bool(p: &mut Parser) -> Res<AstNode> {
	let tok = p.accept_type(TokKind::BoolLit)?;
	Ok(AstNode::BoolLit(
		tok.lexeme
			.parse::<bool>()
			.expect("Could not parse bool (compiler error)"),
	))
}

// ()
fn parse_unit_lit(p: &mut Parser) -> Res<AstNode> {
	p.accept_type(TokKind::LParen)?;
	p.accept_type(TokKind::RParen)?;
	Ok(AstNode::UnitLit)
}

// Single ident
fn parse_ident(p: &mut Parser) -> Res<AstNode> {
	let tok = p.accept_type(TokKind::Ident)?;
	Ok(AstNode::Ident(Ident {
		name: tok.lexeme.clone(),
		token: tok,
	}))
}

// Ident('::' Ident)+
fn parse_namespace(p: &mut Parser) -> Res<NameSpacePath> {
	let mut s = parse_ident(p)?.as_ident_immut().name.clone();
	while p.peek()?.kind == TokKind::DoubleColon {
		p.accept_any()?;
		s += "::";
		s += &parse_ident(p)?.as_ident_immut().name;
	}
	Ok(NameSpacePath::from_str(&s))
}

// Ident | Namespace
fn parse_ident_or_namespace(p: &mut Parser) -> Res<(Option<AstNode>, Option<NameSpacePath>)> {
	if p.place < p.tokens.len() - 1 && p.tokens[p.place + 1].kind == TokKind::DoubleColon {
		Ok((None, Some(parse_namespace(p)?)))
	} else {
		Ok((Some(parse_ident(p)?), None))
	}
}

// Expression Expression
fn parse_application(p: &mut Parser, left: AstNode) -> Res<AstNode> {
	let right = parse_expression(p, Precedence::Application)?;
	Ok(AstNode::Application(Application {
		lhs: Box::new(left),
		rhs: Box::new(right),
	}))
}

// Expression Op Expression
fn parse_binary_operator(p: &mut Parser, left: AstNode) -> Res<AstNode> {
	let curr_prec = p.get_expr_precedence();
	let next = p.peek()?;
	let op_kind = match next.kind {
		TokKind::Add => OpType::Add,
		TokKind::Sub => OpType::Sub,
		TokKind::Mul => OpType::Mul,
		TokKind::Div => OpType::Div,
		TokKind::Mod => OpType::Mod,
		TokKind::CmpEq => OpType::Eq,
		TokKind::CmpNeq => OpType::NEq,
		TokKind::And => OpType::And,
		TokKind::Or => OpType::Or,
		TokKind::Gt => OpType::Gt,
		TokKind::Gte => OpType::Gte,
		TokKind::Lt => OpType::Lt,
		TokKind::Lte => OpType::Lte,
		_ => {
			return Err(ErrMsg::new(&format!(
				"Expected operator, got '{}'",
				next.lexeme
			)))
		}
	};
	let tok = p.accept_any()?;
	let right = parse_expression(p, curr_prec)?;

	Ok(AstNode::BinOp(BinOp {
		token: tok,
		op: op_kind,
		lhs: Box::new(left),
		rhs: Box::new(right),
	}))
}