
use crate::errors::*;
use std::collections::HashMap;


/*
	Most of this is exactly the same as the lexer for the higher level Ast.
	This was the first lexer, and is implemented *only* for lexing Core language files.
	It is not used in the compiler pipeline. However, it will be kept here if support
	is necessary in the future.
*/


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokKind {
	Ident,

	Forall,
	BoolTy,
	NumTy,
	At,
	Fn,
	Adt,

	LParen,
	RParen,
	Colon,

	If,
	Match,
	Case,
	To,
	Let,
	Data,
	Appl,
	TAppl,
	Abst,
	TAbst,
	BoolLit,
	NumLit,
	Prod,

	NAdd,
	NSub,
	NDiv,
	NMul,
	NMod,
	NEq,
	NNeq,
	NGt,
	NGte,
	NLt,
	NLte,
	BEq,
	BNeq,
	BAnd,
	BOr,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub lexeme: String,
	pub kind: TokKind,
	pub line: usize,
	pub col: usize,
}

const OP_CHARS: [char; 17] = [
	'!', '@', '$', '%', '^', '&', '*', '-', '+', '=', ';', '<', '>', '?', '/', '|', '\\',
];

const G_CHARS: [char; 11] = ['(', ')', '{', '}', '[', ']', '"', '\'', ':', ',', '.'];

pub fn lex(input: &str) -> Result<Vec<Token>, ErrMsg> {
	let chars = input.chars().collect::<Vec<char>>();
	let mut pos = 0;
	let mut line = 1;
	let mut col = 1;
	let mut toks: Vec<Token> = Vec::with_capacity(1024);

	let op_map = HashMap::from([("@", TokKind::At)]);

	let g_map = HashMap::from([
		("(", TokKind::LParen),
		(")", TokKind::RParen),
		(":", TokKind::Colon),
	]);

	let k_map = HashMap::from([
		("If", TokKind::If),
		("Match", TokKind::Match),
		("Case", TokKind::Case),
		("To", TokKind::To),
		("true", TokKind::BoolLit),
		("false", TokKind::BoolLit),
		("Bool", TokKind::BoolTy),
		("Num", TokKind::NumTy),
		("Let", TokKind::Let),
		("Data", TokKind::Data),
		("Appl", TokKind::Appl),
		("TAppl", TokKind::TAppl),
		("Abst", TokKind::Abst),
		("TAbst", TokKind::TAbst),
		("Forall", TokKind::Forall),
		("Adt", TokKind::Adt),
		("Fn", TokKind::Fn),
		("Prod", TokKind::Prod),
		("NAdd", TokKind::NAdd),
		("NSub", TokKind::NSub),
		("NMul", TokKind::NMul),
		("NDiv", TokKind::NDiv),
		("NMod", TokKind::NMod),
		("NEq", TokKind::NEq),
		("NNeq", TokKind::NNeq),
		("NGt", TokKind::NGt),
		("NGte", TokKind::NGte),
		("NLt", TokKind::NLt),
		("NLte", TokKind::NLte),
		("BEq", TokKind::BEq),
		("BNeq", TokKind::BNeq),
		("BOr", TokKind::BOr),
		("BAnd", TokKind::BAnd),
	]);

	macro_rules! add_while {
		($cond: expr, $map: expr) => {{
			let mut curr_str = String::new();
			curr_str.push(chars[pos]);
			pos += 1;
			col += 1;
			while pos < chars.len() && $cond {
				curr_str.push(chars[pos]);
				pos += 1;
				col += 1;
			}
			match $map(&curr_str) {
				Some(kind) => Ok(toks.push(Token {
					lexeme: curr_str,
					kind: kind.clone(),
					line,
					col,
				})),
				_ => Err(ErrMsg::new(&format!("Unknown token `{}`", curr_str)).place(line, col)),
			}
		}};
	}

	while pos < chars.len() {
		if chars[pos].is_whitespace() {
			if chars[pos] == '\n' {
				line += 1;
				col = 1;
			} else {
				col += 1;
			}
			pos += 1;
			continue;
		} else if chars[pos].is_ascii_digit()
			|| (pos < chars.len() - 1 && chars[pos] == '.' && chars[pos + 1].is_ascii_digit())
		{
			lex_number(&mut pos, &mut line, &mut col, &chars, &mut toks);
		} else if OP_CHARS.contains(&chars[pos]) {
			add_while!(OP_CHARS.contains(&chars[pos]), |s: &str| op_map.get(s))?;
		} else if G_CHARS.contains(&chars[pos]) {
			let lexeme = chars[pos].to_string();
			let kind = g_map
				.get(&lexeme.as_str())
				.expect("g_map incomplete (this is a compiler error)")
				.clone();
			toks.push(Token {
				lexeme,
				kind,
				line,
				col,
			});
			col += 1;
			pos += 1;
		} else if chars[pos].is_alphabetic() || chars[pos] == '_' {
			add_while!(
				chars[pos].is_alphanumeric() || chars[pos] == '_',
				|s: &str| Some(k_map.get(s).unwrap_or(&TokKind::Ident))
			)?;
		}
	}

	Ok(toks)
}

fn lex_number(
	pos: &mut usize,
	line: &mut usize,
	col: &mut usize,
	chars: &Vec<char>,
	toks: &mut Vec<Token>,
) {
	let mut curr_str = String::new();
	while *pos < chars.len() && chars[*pos].is_ascii_digit() {
		curr_str.push(chars[*pos]);
		*col += 1;
		*pos += 1;
	}

	if *pos < chars.len() - 1 && chars[*pos] == '.' && chars[*pos + 1].is_ascii_digit() {
		curr_str.push(chars[*pos]);
		*pos += 1;
		*col += 1;
		while *pos < chars.len() && chars[*pos].is_ascii_digit() {
			curr_str.push(chars[*pos]);
			*pos += 1;
			*col += 1;
		}
	}
	toks.push(Token {
		lexeme: curr_str,
		kind: TokKind::NumLit,
		line: *line,
		col: *col,
	});
}
