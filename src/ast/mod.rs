use crate::lexer::Token;
use crate::namespace::NameSpacePath;

pub enum AstNode {
	// Basically a File node. All elements of a file (or Module) are stored here.
	Module(Vec<AstNode>),
	// fn _ (...) => ...
	NamedFn(NamedFn),
	// extern ...
	Extern(Extern),
	// data ... | ...
	Data(Data),
	// let ... = ... in ...
	Let(Let),
	// if ... then ... else ...
	If(If),
	// match ... | .. => ..
	Match(Match),
	NumLit(f64),
	BoolLit(bool),
	// ()
	UnitLit,
	// (..., ..., ...)
	Tuple(Vec<AstNode>),
	Ident(Ident),
	// (..., ...) => ...
	Lambda(Lambda),
	#[allow(unused)]
	PrefixOp(PrefixOp),
	// +, -, /, ...
	BinOp(BinOp),
	// function arg
	Application(Application),
}

impl AstNode {
	/// Convert an `AstNode` to the `Ident` variant.
	/// # Panics
	/// Panics if the node is not an `Ident`
	pub fn to_ident(self) -> Ident {
		match self {
			Self::Ident(i) => i,
			_ => panic!("Could not convert to ident (compiler error)"),
		}
	}
	/// Get a mutable reference to an `Ident` variant from an `AstNode`
	/// # Panics
	/// Panics if the node is not an `Ident`
	pub fn as_ident_mut(&mut self) -> &mut Ident {
		match self {
			Self::Ident(i) => i,
			_ => panic!("Could not convert to ident (compiler error)"),
		}
	}
	/// Get an immutable reference to an `Ident` variant from an `AstNode`
	/// # Panics
	/// Panics if the node is not an `Ident`
	pub fn as_ident_immut(&self) -> &Ident {
		match self {
			Self::Ident(i) => i,
			_ => panic!("Could not convert to ident (compiler error)"),
		}
	}
	#[allow(unused)]
	/// Print the Ast
	pub fn print(&self, i: usize) {
		let s = "| ".repeat(i);
		match self {
			Self::Module(c) => {
				for elem in c {
					elem.print(i);
				}
			}
			Self::NamedFn(n) => {
				println!("{}fn {}", s, n.id.as_ident_immut().name);
				n.lambda.print(i + 1);
			}
			Self::Data(d) => {
				let mut arg_str = String::new();
				for arg in &d.args {
					arg_str += &arg.as_ident_immut().name;
					arg_str += ", ";
				}
				if arg_str.is_empty() {
					arg_str = String::from("  ");
				}
				println!(
					"{}data {} {}",
					s,
					d.id.as_ident_immut().name,
					&arg_str[0..arg_str.len() - 2]
				);
				for cons in &d.constructors {
					println!(
						"| {}{}: {}",
						s,
						cons.0.as_ident_immut().name,
						cons.1.to_str()
					)
				}
			}
			Self::Extern(e) => {
				println!("{} extern {}: {}", s, e.id.as_ident_immut().name, e.ty.to_str())
			}
			Self::Let(l) => {
				println!("{}let {}", s, l.id.as_ident_immut().name);
				l.lhs.print(i + 1);
				l.rhs.print(i + 1);
			}
			Self::If(istmt) => {
				println!("{}if", s);
				istmt.condition.print(i + 1);
				istmt.t_stmt.print(i + 1);
				istmt.f_stmt.print(i + 1);
			}
			Self::Match(m) => {
				println!("{}match", s);
				m.expr.print(i + 1);
				for arm in &m.arms {
					println!("| {}Pattern: {}", s, arm.0.to_str());
					if let Some(g) = &arm.1 {
						g.print(i + 2);
					}
					arm.2.print(i + 2);
				}
			}
			Self::NumLit(n) => println!("{}{}", s, n),
			Self::BoolLit(b) => println!("{}{}", s, b),
			Self::UnitLit => println!("{}()", s),
			Self::Lambda(l) => {
				let mut arg_str = String::new();
				for arg in &l.args {
					arg_str += &format!("{}: {}", arg.0.as_ident_immut().name, arg.1.to_str());
					arg_str += ", ";
				}
				if arg_str.is_empty() {
					arg_str += "  ";
				}
				println!(
					"{}({}): {} => ",
					s,
					&arg_str[0..arg_str.len() - 2],
					l.ret_type.to_str()
				);
				l.body.print(i + 1);
			}
			Self::Tuple(args) => {
				println!("{}Tuple", s);
				for arg in args {
					arg.print(i + 1);
				}
			}
			Self::Ident(i) => println!("{}{}", s, i.name),
			Self::PrefixOp(p) => {
				println!("{}{:?}", s, p.op);
				p.arg.print(i + 1);
			}
			Self::BinOp(b) => {
				println!("{}{:?}", s, b.op);
				b.lhs.print(i + 1);
				b.rhs.print(i + 1);
			}
			Self::Application(a) => {
				println!("{}Application", s);
				a.lhs.print(i + 1);
				a.rhs.print(i + 1);
			}
		}
	}
}

#[derive(Clone)]
pub enum Type {
	// ()
	Unit,
	Num,
	Bool,
	// Generic type
	TypeVar(Ident),
	// AdtName arg1 arg2 ... argn
	AdtType(AdtType),
	// a -> b
	FnType(FnType),
	// (a, b, ..., n)
	ProdType(ProdType),
}

impl Type {
	#[allow(unused)]
	/// Convert a type to a pretty string representation
	pub fn to_str(&self) -> String {
		match self {
			Self::Unit => "()".to_string(),
			Self::Num => "Num".to_string(),
			Self::Bool => "Bool".to_string(),
			Self::TypeVar(i) => i.name.clone(),
			Self::AdtType(a) => {
				let mut s = String::new();
				for arg in &a.args {
					s += &arg.to_str();
					s += " ";
				}
				format!("{} {}", a.ident.name, s)
			}
			Self::FnType(f) => format!("({} => {})", f.lhs.to_str(), f.rhs.to_str()),
			Self::ProdType(p) => {
				let mut s = String::from("(");
				for item in &p.args {
					s += &item.to_str();
					s += ", ";
				}

				s[..s.len() - 2].to_string() + ")"
			}
		}
	}
}

pub enum Pattern {
	// a, b, ...
	Any(Ident),
	// AdtName arg1 arg2 pattern
	Adt {
		ident: Ident,
		args: Vec<Type>,
		pattern: Box<Pattern>,
	},
	Num(f64),
	Bool(bool),
	Unit,
	// (pattern, pattern, ...)
	Tuple(Vec<Pattern>),
}

impl Pattern {
	#[allow(unused)]
	/// Convert a pattern to a pretty string
	pub fn to_str(&self) -> String {
		match self {
			Self::Any(i) => format!("{}", i.name),
			Self::Adt {
				ident,
				args,
				pattern,
			} => {
				let mut s = String::new();
				for arg in args {
					s.push_str(&(arg.to_str() + " "));
				}
				format!("{}:{} {}", ident.name, s, pattern.to_str())
			}
			Self::Num(n) => format!("{}", n),
			Self::Bool(b) => format!("{}", b),
			Self::Unit => format!("()"),
			Self::Tuple(t) => {
				let mut s = String::from("(");
				for p in t {
					s.push_str(&(p.to_str() + ", "))
				}
				s[0..s.len() - 2].to_string() + ")"
			}
		}
	}
}

#[derive(Debug)]
pub enum OpType {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Eq,
	NEq,
	And,
	Or,
	Gt,
	Gte,
	Lt,
	Lte,
}

pub struct NamedFn {
	pub qualified_name: Option<NameSpacePath>,
	pub token: Token,
	pub id: Box<AstNode>,
	pub lambda: Box<AstNode>,
}

pub struct Data {
	pub token: Token,
	pub id: Box<AstNode>,
	pub args: Vec<AstNode>,
	pub constructors: Vec<(AstNode, Type)>,
}

pub struct Extern {
	pub token: Token,
	pub id: Box<AstNode>,
	pub ty: Type,
}

pub struct Let {
	pub token: Token,
	pub id: Box<AstNode>,
	pub hint: Type,
	pub lhs: Box<AstNode>,
	pub rhs: Box<AstNode>,
}

pub struct If {
	pub token: Token,
	pub condition: Box<AstNode>,
	pub t_stmt: Box<AstNode>,
	pub f_stmt: Box<AstNode>,
}

pub struct Match {
	pub token: Token,
	pub expr: Box<AstNode>,
	pub arms: Vec<(Pattern, Option<AstNode>, AstNode)>,
}

#[derive(Clone)]
pub struct Ident {
	pub token: Token,
	pub name: String,
}

pub struct PrefixOp {
	pub token: Token,
	pub op: OpType,
	pub arg: Box<AstNode>,
}

pub struct BinOp {
	pub token: Token,
	pub op: OpType,
	pub lhs: Box<AstNode>,
	pub rhs: Box<AstNode>,
}

pub struct Application {
	pub lhs: Box<AstNode>,
	pub rhs: Box<AstNode>,
}

#[derive(Clone)]
pub struct AdtType {
	pub ident: Ident,
	pub args: Vec<Type>,
}

#[derive(Clone)]
pub struct FnType {
	pub lhs: Box<Type>,
	pub rhs: Box<Type>,
}

#[derive(Clone)]
pub struct ProdType {
	pub args: Vec<Type>,
}

pub struct Lambda {
	pub args: Vec<(AstNode, Type)>,
	pub ret_type: Type,
	pub body: Box<AstNode>,
}
