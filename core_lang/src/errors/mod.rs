use std::error::Error;

/// Wrapper around `Result` type; expands to `Result<T, ErrMsg>`
pub type Res<T> = Result<T, ErrMsg>;

pub struct ErrMsg {
	pub msg: String,
	pub line: Option<usize>,
	pub col: Option<usize>,
}

impl ErrMsg {
	pub fn new(msg: &str) -> ErrMsg {
		ErrMsg {
			msg: msg.to_owned(),
			line: None,
			col: None,
		}
	}
	/// Add a file location to the error message
	pub fn place(self, line: usize, col: usize) -> ErrMsg {
		ErrMsg {
			msg: self.msg,
			line: Some(line),
			col: Some(col),
		}
	}
}

impl std::fmt::Debug for ErrMsg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let place = if let (Some(l), Some(c)) = (self.line, self.col) {
			format!("(Line {}, Column {})", l, c)
		} else {
			"".to_owned()
		};

		f.write_str(&format!("Error: {} {}", self.msg, place))
	}
}

impl std::fmt::Display for ErrMsg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(&format!("{:?}", self))
	}
}

impl Error for ErrMsg {}
