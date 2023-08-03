use crate::lexer::Token;

// Cast an AstNode into the type that we want
macro_rules! cast {
	($ast: expr, $p: path) => {{
		match $ast {
			$p(x) => x,
			_ => panic!("Could not cast {}", stringify!($ast)),
		}
	}};
}

#[derive(Clone)]
/// Basic nodes for the Core language.
pub enum Ast {
	// List of top level nodes
	Module(Vec<Ast>),
	// External function
	Extern(Extern),
	// Application
	Appl(Appl),
	// Type application
	TAppl(TAppl),
	// Abstraction
	Abst(Abst),
	// Type Abstraction
	TAbst(TAbst),
	Ident(Ident),
	If(If),
	Match(Match),
	Let(Let),
	// A constant, top level function
	Const(Const),
	Data(Data),
	Constructor(Constructor),
	NumLit(NumLit),
	UnitLit,
	BoolLit(BoolLit),
	ProdLit(ProdLit),
	BinOp(BinOp),
	// To avoid copies later
	None,
}

#[derive(Clone)]
/// Primary nodes for the Core Type
pub enum Type {
	None,
	Unit,
	Bool,
	Num,
	Kind,
	TypeVar(TypeVar),
	Fn(Fn),
	Prod(Prod),
	Adt(Adt),
	Universal(Universal),
}

#[derive(Clone)]
/// Patterns for Core pattern matching
pub enum Pattern {
	Any(Ident),
	Adt(Ident, Vec<Type>, Box<Pattern>),
	Num(NumLit),
	Bool(BoolLit),
	Unit,
	Prod(Vec<Pattern>),
}

impl Pattern {
	/// Convert a pattern into a pretty string
	pub fn to_str(&self) -> String {
		match self {
			Self::Any(i) => i.name.clone(),
			Self::Adt(i, _, p) => format!("{} {}", i.name, p.to_str()),
			Self::Num(n) => format!("{}", n.val),
			Self::Unit => format!("()"),
			Self::Bool(b) => format!("{}", b.val),
			Self::Prod(p) => {
				let mut res = String::from("(");
				for item in p {
					res += &item.to_str();
					res += ", ";
				}
				res[..res.len() - 2].to_string() + ")"
			}
		}
	}
}

impl Ast {
	/// Pretty-print an AST
	pub fn print(&self, i: usize) {
		let s = "| ".repeat(i) + "+ ";
		match self {
			Ast::Module(m) => {
				for node in m { node.print(0) }
			}
			Ast::Extern(e) => {
				println!("{} Extern {}", s, cast!(e.id.as_ref(), Ast::Ident).name);
				for a in &e.args { a.print(i + 1) }
			}
			Ast::Appl(a) => {
				println!("{} Appl: {}", s, a.kind.to_str());
				a.left.print(i + 1);
				a.right.print(i + 1);
			}
			Ast::TAppl(t) => {
				println!("{} Type Appl: {}", s, t.kind.to_str());
				t.left.print(i + 1);
				println!("| {} {}", s, t.right.to_str())
			}
			Ast::Abst(a) => {
				println!("{} Lambda: {}", s, a.kind.to_str());
				a.ident.print(i + 1);
				println!("| {} Hint: {}", s, a.type_hint.to_str());
				a.body.print(i + 1);
			}
			Ast::TAbst(a) => {
				println!("{} Type Lambda: {}", s, a.kind.to_str());
				a.body.print(i + 1);
			}
			Ast::Ident(id) => {
				println!("{} Ident {}: {}", s, id.name, id.kind.to_str());
			}
			Ast::If(ifstmt) => {
				println!("{} If: {}", s, ifstmt.kind.to_str());
				ifstmt.condition.print(i + 1);
				ifstmt.t_stmt.print(i + 1);
				ifstmt.f_stmt.print(i + 1);
			}
			Ast::Match(m) => {
				println!("{} Match: {}", s, m.kind.to_str());
				m.lhs.print(i + 1);
				for arm in &m.arms {
					println!("{} | Case:  {}", s, &arm.0.to_str());
					if let Some(g) = &arm.1 {
						g.print(i + 1);
					}
					arm.2.print(i + 1);
				}
			}
			Ast::Let(l) => {
				println!("{} Let: {}", s, l.kind.to_str());
				l.id.print(i + 1);
				println!("| {} Hint: {}", s, l.type_hint.to_str());
				l.lhs.print(i + 1);
				l.rhs.print(i + 1);
			}
			Ast::Const(c) => {
				println!("{} Const: {}", s, c.kind.to_str());
				c.id.print(i + 1);
				c.rhs.print(i + 1);
			}
			Ast::Data(d) => {
				println!("{} Data: {}", s, d.kind.to_str());
				d.id.print(i + 1);
				for c in &d.constructors {
					c.print(i + 1);
				}
				if let Some(rhs) = &d.rhs {
					rhs.print(i + 1);
				}
			}
			Ast::BinOp(b) => {
				println!("{} Op: {:?}", s, b.op_ty);
				b.lhs.print(i + 1);
				b.rhs.print(i + 1);
			}
			Ast::Constructor(c) => {
				println!("{} Constructor", s);
				c.id.print(i + 1);
				println!("| {} {}", s, c.kind.to_str())
			}
			Ast::NumLit(n) => println!("{} {}", s, n.val),
			Ast::BoolLit(b) => println!("{} {}", s, b.val),
			Ast::UnitLit => println!("{}()", s),
			Ast::ProdLit(prod) => {
				println!("{} Tuple", s);
				for item in &prod.items {
					item.print(i + 1);
				}
			}
			Ast::None => println!("{} None", s),
		}
	}
}

impl Type {
	/// Convert a type into a pretty string
	pub fn to_str(&self) -> String {
		match self {
			Type::None => "None".to_string(),
			Type::Bool => "Bool".to_string(),
			Type::Num => "Num".to_string(),
			Type::Unit => "()".to_string(),
			Type::Kind => "*".to_string(),
			Type::TypeVar(i) => i.id.to_string(),
			Type::Fn(f) => format!("({} -> {})", f.lhs.to_str(), f.rhs.to_str()),
			Type::Prod(p) => {
				format!(
					"({})",
					p.items.iter().fold(String::new(), |x, y| {
						let empty = x.is_empty();
						x + if empty { "" } else { ", " } + &y.to_str()
					})
				)
			}
			Type::Adt(a) => {
				format!(
					"({} {})",
					&cast!(a.id.as_ref(), Ast::Ident).name,
					a.args
						.iter()
						.fold(String::new(), |x, y| x + " " + &y.to_str())
				)
			}
			Type::Universal(u) => {
				format!("∀{}.{}", u.arg.to_str(), u.body.to_str())
			}
		}
	}
}

#[derive(Clone)]
pub struct Extern {
	pub id: Box<Ast>,
	pub args: Vec<Ast>,
	pub ret_ty: Type,
}

#[derive(Clone)]
pub struct Fn {
	pub lhs: Box<Type>,
	pub rhs: Box<Type>,
}

#[derive(Clone)]
pub struct Prod {
	pub items: Vec<Type>,
}

#[derive(Clone)]
pub struct Adt {
	pub id: Box<Ast>,
	pub args: Vec<Type>,
}

#[derive(Clone)]
pub struct TypeVar {
	pub tok: Token,
	pub id: String,
}

#[derive(Clone)]
pub struct Abst {
	pub kind: Type,
	pub type_hint: Type,
	pub ident: Box<Ast>,
	pub body: Box<Ast>,
}

#[derive(Clone)]
pub struct TAbst {
	pub kind: Type,
	pub ident: Type,
	pub body: Box<Ast>,
}

#[derive(Clone)]
pub struct Ident {
	pub kind: Type,
	pub name: String,
	pub token: Token,
}

#[derive(Clone)]
pub struct Appl {
	pub kind: Type,
	pub left: Box<Ast>,
	pub right: Box<Ast>,
}

#[derive(Clone)]
pub struct TAppl {
	pub kind: Type,
	pub left: Box<Ast>,
	pub right: Type,
}

#[derive(Clone)]
pub struct If {
	pub kind: Type,
	pub condition: Box<Ast>,
	pub t_stmt: Box<Ast>,
	pub f_stmt: Box<Ast>,
}

#[derive(Clone)]
pub struct Match {
	pub kind: Type,
	pub lhs: Box<Ast>,
	// (Pattern, Guard, Expression)
	pub arms: Vec<(Pattern, Option<Ast>, Ast)>
}

#[derive(Clone)]
pub struct Let {
	pub kind: Type,
	pub type_hint: Type,
	pub id: Box<Ast>,
	pub lhs: Box<Ast>,
	pub rhs: Box<Ast>,
}

#[derive(Clone)]
pub struct Const {
	// Only exported constant functions have fully qualified names.
	// We keep them around so that we can call these functions easily from Rust.
	pub fully_qualified_name: Option<String>,
	pub kind: Type,
	pub id: Box<Ast>,
	pub hint: Type,
	pub rhs: Box<Ast>,
}

#[derive(Clone)]
pub struct Data {
	pub id: Box<Ast>,
	pub kind: Type,
	pub args: Vec<Type>,
	pub constructors: Vec<Ast>,
	// While it is legal for Core data to have a right hand side,
	// this is not the case for the higher level AST. So we may
	// not have anything to compile here.
	pub rhs: Option<Box<Ast>>,
}

#[derive(Clone)]
pub struct Constructor {
	pub id: Box<Ast>,
	pub kind: Type,
	pub data_type: Type,
	pub tag: u64,
}

#[derive(Clone)]
pub struct NumLit {
	pub kind: Type,
	pub val: f64,
}

#[derive(Clone)]
pub struct BoolLit {
	pub kind: Type,
	pub val: bool,
}

#[derive(Clone)]
pub struct ProdLit {
	pub items: Vec<Ast>,
}

#[derive(Clone)]
pub struct Universal {
	// Universal type (like a function type, except with type arguments)
	pub arg: Box<Type>,
	pub body: Box<Type>,
}


#[derive(Clone, Debug)]
pub enum OpTy {
	NumAdd,
	NumSub,
	NumDiv,
	NumMul,
	NumMod,
	NumEq,
	NumNeq,
	NumGt,
	NumGte,
	NumLt,
	NumLte,
	BoolEq,
	BoolNeq,
	BoolAnd,
	BoolOr,
}

#[derive(Clone)]
pub struct BinOp {
	pub kind: Type,
	pub op_ty: OpTy,
	pub lhs: Box<Ast>,
	pub rhs: Box<Ast>,
}
