use super::values::Value;

#[derive(Debug, Clone)]
/// A simple, stack-based instruction set serving as a compilation target for the Core language.
pub enum Bytecodes {
	// Stack
	// Push value to stack
	Push(Value),
	// Duplicate top stack value
	Duplicate,
	// Pop value from stack
	Pop,
	// Pop n items from the stack. N is top of stack.
	DynPop,
	// Copy item at arg to top of stack
	GetLoc(u64),
	// Pop stack and move it to location
	SetLoc(u64),
	// Copy item from environment to top of stack
	GetEnv(u64),
	// Pop stack and move to location
	SetEnv(u64),

	// Control Flow
	// Set PC to value
	Jmp(u64),
	// Pop stack (a), pop again (b)
	// Pop stack. If false, jump to val
	Jz(u64),
	// Pop stack. If true, jump to val
	Jnz(u64),
	// Top of stack should be function object. Call it.
	// Second to top is argument.
	// Set stack frame.
	Call,
	// Call extern with specified args
	// Stack: First arg, second arg, ...
	CallExtern(String, u64),
	// Clear stack frame. Return value is at top of stack.
	Ret,

	// Pop rhs, pop lhs. Perform operation.
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Eq,
	Neq,
	Gt,
	Gte,
	Lt,
	Lte,
	And,
	Or,

	// Data Constructors
	// Push item to stack
	MkNum(f64),
	MkBool(bool),
	// Pop n elems from stack and arrange into tuple
	// (Top, 2nd, 3rd, ...)
	MkProd(u64),
	// Push product values to stack. Top is first, next is 2nd, ...
	DeconsProd,
	// Arg is tag. Top of stack is data for variant. Pop this, push variant
	MkVariant(u64),
	// Push data THEN tag to stack. Tag is top.
	DeconsVariant,
	// Pop n items from stack to make environment. Stack = {1st, 2nd, 3rd, ...} from top -> bottom
	// Second arg is fn pointer. Combine and push closure to stack.
	MkClosure(u64, u64),

	// Do nothing
	Pass,
	// Runtime Error
	RuntimeErr(String),
	// Stop execution
	Hlt,
}
