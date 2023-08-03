#[derive(Debug, Clone)]
/// A basic, fundamental value used to implement the Core language.
pub enum Value {
	Num(f64),
	Bool(bool),
	Prod(Box<Vec<Value>>),
	Variant { tag: u64, data: Box<Value> },
	Closure(Box<Closure>),
	None,
}

#[derive(Debug, Clone)]
pub struct Closure {
	pub env: Vec<Value>,
	// Function pointer
	pub func: u64,
}

#[derive(Debug, Clone)]
pub struct Variant {
	pub tag: u32,
	pub data: Box<Value>,
}
