/*
 *  Major Types
 *      Idents/Keywords
 *          (a-zA-Z_)+(a-zA-Z0-9_)
 *      Numbers
 *          (0-9)* .? (0-9)+
 *      Operators
 *          !@$%^&*-+=;<>?/|
 *      Grammar
 *          (){}[]"':,.
 *      Special
 *          #
 */

use core_lang::errors::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokKind {
	// Keywords
	Import,
	Export,
	Extern,
	Let,
	In,
	Fn,
	Data,
	If,
	Then,
	Else,
	Match,

	// Literals
	BoolLit,
	NumLit,
	Ident,

	// Types
	BoolType,
	NumType,

	// Operators
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	CmpEq,
	CmpNeq,
	And,
	Or,
	Gt,
	Gte,
	Lt,
	Lte,

	// Symbols
	Equals,
	Bar,
	ThickArrow,
	ThinArrow,

	// Grammar
	LParen,
	RParen,
	Comma,
	Colon,
	DoubleColon,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub lexeme: String,
	pub kind: TokKind,
	pub line: usize,
	pub col: usize,
}

impl Token {
	/// Convert a token to the core token type
	pub fn to_core_tok(self) -> core_lang::lexer::Token {
		core_lang::lexer::Token {
			lexeme: self.lexeme,
			kind: core_lang::lexer::TokKind::Ident,
			line: self.line,
			col: self.col,
		}
	}
}

/// Any character that can be used as an operator. During lexing, longest matching strings
/// of these characters will be classified as an operator. For example, lexing `"++"` results
/// in one single operator (`++`), but lexing `"+ +"` results in two (`+` and `+`).
const OP_CHARS: [char; 18] = [
	'!', '@', '$', '%', '^', '&', '*', '-', '+', '=', ';', '<', '>', '?', '/', '|', '\\', ':',
];

/// Any character that can be used as Grammar. During lexing, only a single one of these characters
/// can ever match in a row. For example, `"((("` is lexed into three separate `(` tokens, regardless of whitespace.
const G_CHARS: [char; 10] = ['(', ')', '{', '}', '[', ']', '"', '\'', ',', '.'];

/// Lex an `input` string into a vector of Tokens.
/// # Errors
/// Returns an error if the lexer hits an unregistered initial character.
pub fn lex(input: &str) -> Result<Vec<Token>, ErrMsg> {
	let chars = input.chars().collect::<Vec<char>>();
	let mut pos = 0;
	let mut line = 1;
	let mut col = 1;
	let mut toks: Vec<Token> = Vec::with_capacity(1024);

	// Map every operator character to its corresponding token type
	let op_map = HashMap::from([
		("=", TokKind::Equals),
		("|", TokKind::Bar),
		("=>", TokKind::ThickArrow),
		("->", TokKind::ThinArrow),
		("+", TokKind::Add),
		("-", TokKind::Sub),
		("*", TokKind::Mul),
		("/", TokKind::Div),
		("%", TokKind::Mod),
		("==", TokKind::CmpEq),
		("!=", TokKind::CmpNeq),
		("&&", TokKind::And),
		("||", TokKind::Or),
		(">", TokKind::Gt),
		(">=", TokKind::Gte),
		("<", TokKind::Lt),
		("<=", TokKind::Lte),
		(":", TokKind::Colon),
		("::", TokKind::DoubleColon),
	]);

	// Map every grammar character to its corresponding token type
	let g_map = HashMap::from([
		("(", TokKind::LParen),
		(")", TokKind::RParen),
		(",", TokKind::Comma),
	]);

	// Map every keyword to its corresponding token type
	let k_map = HashMap::from([
		("import", TokKind::Import),
		("export", TokKind::Export),
		("extern", TokKind::Extern),
		("if", TokKind::If),
		("then", TokKind::Then),
		("else", TokKind::Else),
		("match", TokKind::Match),
		("true", TokKind::BoolLit),
		("false", TokKind::BoolLit),
		("Bool", TokKind::BoolType),
		("Num", TokKind::NumType),
		("let", TokKind::Let),
		("in", TokKind::In),
		("data", TokKind::Data),
		("fn", TokKind::Fn),
	]);

	/// Add a character to an accumulator string while some condition is met
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
		// Skip whitespace, update line/col
		if chars[pos].is_whitespace() {
			if chars[pos] == '\n' {
				line += 1;
				col = 1;
			} else {
				col += 1;
			}
			pos += 1;
			continue;
		}

		// Digits, or single periods followed by digits, should be lexed as numbers.
		else if chars[pos].is_ascii_digit()
			|| (pos < chars.len() - 1 && chars[pos] == '.' && chars[pos + 1].is_ascii_digit())
		{
			lex_number(&mut pos, &mut line, &mut col, &chars, &mut toks);
		}

		// Is the character an operator?
		else if OP_CHARS.contains(&chars[pos]) {
			add_while!(OP_CHARS.contains(&chars[pos]), |s: &str| op_map.get(s))?;
		}

		// Is the character grammar?
		else if G_CHARS.contains(&chars[pos]) {
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
		}

		// Is the character an identifier?
		else if chars[pos].is_alphabetic() || chars[pos] == '_' {
			add_while!(
				chars[pos].is_alphanumeric() || chars[pos] == '_',
				|s: &str| Some(k_map.get(s).unwrap_or(&TokKind::Ident))
			)?;
		}

		// If nothing, report an error.
		else {
			return Err(ErrMsg::new(&format!("Unknown character `{}`", chars[pos])))
		}
	}

	Ok(toks)
}

/// Lex a number of the form 0-9* .? 0-9+
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
