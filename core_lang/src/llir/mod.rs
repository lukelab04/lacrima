use std::collections::HashMap;

use crate::vm::{values::Value, bytecode::Bytecodes};

/// LLIR (Low Level Intermediate Representation) is an assembly-like languages
/// with several higher level quality of life changes. In general, while
/// using LLIR, the programmer does not have to worry about captured variables,
/// memory locations, stack management, etc. -- it will be handled automatically.
///
/// LLIR is a stack based language, and does not have registers.
pub enum LLIR {
	// Call with x args. Set up frame, including return address.
	// Top of stack is Environment, which contains pointer to fn
	Call(u64),
	// Call an external function with specified number of args
	CallExtern(String, u64),
	// Clear stack frame and return. Item on top of stack is return value.
	Ret,
	// Make an envirnment for function name.
	// Infer which variables need to be moved from the function name.
	MakeEnv(String),
	// Load a variable and clone its value to the top of the stack
	Load(String),
	// Pop stack, move value to variable
	Set(String),
	// Make a label (should be unique)
	Label(String),
	// If arg on top of stack is false, goto label
	GotoIfFalse(String),
	// Goto label
	Goto(String),
	// Declare variable with a name and initialize with top stack value
	VarDecl(String),
	// Declare function with name and args
	FnDecl(String, Vec<String>),
	// Push Value to stack
	Push(Value),
	// Pop value from stack
	Pop,
	// Pop a variable number of items from the stack
	DynPop,
	// Duplicate the top stack value
	Duplicate,
	// Make a product datatype with x args from stack
	// Stack top->bottom = {first elem, second elem, ...}
	MkProd(u64),
	// Deconstruct product. Put first elem at top of stack, then next, then next...
	DeconsProd,
	// Make a Variant datatype with tag x. Data is on top of stack.
	MkVariant(u64),
	// Deconstruct variant. Tag is top of stack, data is second to top.
	DeconsVariant,

	// Pop rhs, pop lhs, perform operation
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

	// Exit with an error
	RuntimeErr(String),
}

impl LLIR {
	#[allow(unused)]
	/// Pretty-print LLIR
	pub fn print(&self) {
		match self {
			LLIR::Call(_) => println!("\tCall"),
			LLIR::CallExtern(s, a) => println!("\tCall {} with {} args", s, a),
			LLIR::Ret => println!("\tRet"),
			LLIR::MakeEnv(s) => println!("\tMake closure for {s}"),
			LLIR::Load(l) => println!("\tLoad {l}"),
			LLIR::Set(s) => println!("\tSet {s}"),
			LLIR::Label(l) => println!("  Label {l}"),
			LLIR::GotoIfFalse(f) => println!("\tIf false, goto {f}"),
			LLIR::Goto(f) => println!("\tGoto {f}"),
			LLIR::VarDecl(v) => println!("\tVar: {v}"),
			LLIR::FnDecl(f, _) => println!("Fn {f}"),
			LLIR::Push(p) => println!("\tPush {:?}", p),
			LLIR::Pop => println!("\tPop"),
			LLIR::DynPop => println!("\tDynamic Pop"),
			LLIR::Duplicate => println!("\tDuplicate"),
			LLIR::MkProd(u) => println!("\tProd({u})"),
			LLIR::DeconsProd => println!("\tDeconstruct Product"),
			LLIR::MkVariant(v) => println!("\tVariant {v}"),
			LLIR::DeconsVariant => println!("\tDeconstruct Variant"),
			LLIR::Add => println!("\tAdd"),
			LLIR::Sub => println!("\tSub"),
			LLIR::Mul => println!("\tMul"),
			LLIR::Div => println!("\tDiv"),
			LLIR::Mod => println!("\tMod"),
			LLIR::Eq => println!("\tEq"),
			LLIR::Neq => println!("\tNeq"),
			LLIR::Gt => println!("\tGt"),
			LLIR::Gte => println!("\tGte"),
			LLIR::Lt => println!("\tLt"),
			LLIR::Lte => println!("\tLte"),
			LLIR::And => println!("\tAnd"),
			LLIR::Or => println!("\tOr"),
			LLIR::RuntimeErr(e) => println!("\tRuntime Error: {}", e)
		}
	}
}

/// Data structure to keep track of which variable names
/// correspond to which addresses. Additionally, captured
/// closure arguments are stored for easy lookup. The primary
/// purpose of this data structure is to make it easy for the programmer
/// to query for the memory location of named variables.
struct VarManager {
	// This is a stack based language, so we should keep track of the next free stack memory address
	addr: Vec<u64>,
	locals: HashMap<String, u64>,
	// Function name -> (Captured Arg, Captured Arg Addr)
	globals: HashMap<String, HashMap<String, u64>>,
	curr_fn: String,
}

impl VarManager {
	pub fn new() -> VarManager {
		VarManager { addr: vec![0], locals: HashMap::new(), globals: HashMap::new(), curr_fn: String::new() }
	}
	/// Enter function scope.
	pub fn push_scope(&mut self, fn_name: &str, locals: HashMap<String, u64>) {
		self.curr_fn = fn_name.to_owned();
		// The frame is reset inside a function, so memory restarts back a 0
		self.addr.push(0);
		self.locals = locals;
	}
	/// Exit function scope.
	pub fn pop_scope(&mut self) {
		self.addr.pop();
		self.locals.clear();
	}
	/// Register a function and its captured variables.
	/// # Panics
	/// Panics if the function was already registered.
	pub fn register_fn(&mut self, name: &str, captured: HashMap<String, u64>) {
		match self.globals.insert(name.to_owned(), captured) {
			Some(_) => panic!("Name {name} has already been registered (compiler error)"),
			_ => ()
		}
	}
	/// From the name of a function, retrieve its captured variables.
	/// # Panics
	/// Panics if the name specified was not registered as a function.
	pub fn get_fn(&self, name: &str) -> &HashMap<String, u64> {
		match self.globals.get(name) {
			Some(g) => g,
			_ => panic!("Name {name} is not a global value (compiler error)"),
		}
	}
	/// Query whether the variable is local. If not, it must be captured.
	pub fn is_local(&self, name: &str) -> bool {
		self.locals.contains_key(name)
	}
	/// Get the variable address for some identifier.
	/// If the variable is non-local, get its environment address.
	/// # Panics
	/// Panics if the variable was not locally or environmentally declared.
	pub fn get_var_addr(&self, id: &str) -> u64 {
		if let Some(addr) = self.locals.get(id) {
			// If local, return that address.
			*addr
		} else {
			let fn_info = self.globals.get(&self.curr_fn).expect("Error getting current function (compiler error)");
			match fn_info.get(id) {
				// If captured, return the environment address.
				Some(addr) => *addr,
				_ => panic!("Id {id} is neither local or captured (compiler error)"),
			}
		}
	}
}

/// Given LLIR code and a map from qualified names to unique names, compile into bytecode and a map from qualified function names to addresses.
pub fn to_bytecode(llir: &mut Vec<LLIR>, qual_to_unique: HashMap<String, String>) -> (Vec<Bytecodes>, HashMap<String, u64>) {
	let lbc = llir_to_lbc(llir, &mut VarManager::new());
	lbc_to_bc(lbc, qual_to_unique)
}

/// LBC (Labelled Byte Code) is an intermediate form between LLIR and Bytecode.
/// LBC does not offer some of the conveniences of LLIR (variables are not named, for example)
/// but it does offer labels, as the name would suggest. These labels make it significantly
/// easier for the programmer to create loops, control flow, and function calls. Label order
/// does not matter; jumping to labels further down the code is perfectly legal and well-defined.
enum LBC {
	// LBC is really just a thin layer around Bytecode, and a layer that many instructions don't need.
	BC(Bytecodes),
	// Function name or actual label
	LabelDecl(String),
	// Num args, fn name
	Closure(u64, String),
	GotoLabel(String),
	GotoIfFalse(String),
}

/// Convert LLIR to LBC.
fn llir_to_lbc(llir: &mut Vec<LLIR>, v: &mut VarManager) -> Vec<LBC> {
	let mut idx = 0;
	let mut code = vec![];

	// Collect all of the function names so we can jump to them from any point in the code.
	register_fns(llir, v);

	while idx < llir.len() {
		// Many LLIR instructions are 1:1 with Bytecode. These can be easily translated.
		match &llir[idx] {
			LLIR::Call(_) => code.push(LBC::BC(Bytecodes::Call)),
			LLIR::CallExtern(s, a) => code.push(LBC::BC(Bytecodes::CallExtern(s.clone(), *a))),
			LLIR::Ret => {
				// We need to pop our variable scope, because a function just ended.
				v.pop_scope();
				code.push(LBC::BC(Bytecodes::Ret))
			}
			LLIR::MakeEnv(fn_name) => {
				// In Bytecode, we make an environment by pushing all the captured variables
				// onto the stack, and then collecting them into the environment. There are
				// a few nuanced pieces to this. Firstly, we must do it in a very specific order;
				// The variables will have a defined memory order within the environment. We accomplish this
				// by sorting from largest to smallest address (things will be reversed once pushed onto the stack).
				// Additionally, when getting our variables onto the stack in the first place, we must pay
				// careful attention as to whether they are local or environmental variables, and treat
				// them accordingly.
				let fn_attribs = v.get_fn(&fn_name).clone();
				let mut fn_attribs = fn_attribs.into_iter().map(|(key, val)| (key, val)).collect::<Vec<(String, u64)>>();
				// Sort from largest address to smallest
				fn_attribs.sort_by(|(_, a), (_, b)| b.cmp(a));
				for item in &fn_attribs {
					let addr = v.get_var_addr(&item.0);
					if v.is_local(&item.0) {
						code.push(LBC::BC(Bytecodes::GetLoc(addr)))
					} else {
						code.push(LBC::BC(Bytecodes::GetEnv(addr)))
					}
				}
				// Make the closure
				code.push(LBC::Closure(fn_attribs.len() as u64, fn_name.clone()));
			}
			// The only slight complication to this is deciding whether a
			// variable should be loaded locally from the stack or from the environment.
			LLIR::Load(name) => {
				let addr = v.get_var_addr(&name);
				if v.is_local(name) {
					code.push(LBC::BC(Bytecodes::GetLoc(addr)))
				} else {
					code.push(LBC::BC(Bytecodes::GetEnv(addr)))
				}
			}
			// As with Load, we need to be careful with local vs. environment.
			LLIR::Set(name) => {
				let addr = v.get_var_addr(&name);
				if v.is_local(name) {
					code.push(LBC::BC(Bytecodes::SetLoc(addr)))
				} else {
					code.push(LBC::BC(Bytecodes::SetEnv(addr)))
				}
			}
			LLIR::Label(name) => code.push(LBC::LabelDecl(name.clone())),
			LLIR::GotoIfFalse(name) => code.push(LBC::GotoIfFalse(name.clone())),
			LLIR::Goto(name) => code.push(LBC::GotoLabel(name.clone())),
			LLIR::VarDecl(name) => {
				code.push(LBC::BC(Bytecodes::SetLoc(v.get_var_addr(&name))));
			}
			LLIR::FnDecl(f, args) => {
				// We already registered our function, so all we need to do is
				// actually declare the label, alloc args, and compile the body

				// Set label
				code.push(LBC::LabelDecl(f.clone()));
				// Alloc space
				let (local_vars, _) = process_vars(idx, llir, &args);
				let len = local_vars.len();
				// Arguments are pushed onto the stack when the function is called.
				// So we do not have to allocate any space for them.
				// We do need to allocate space for local_vars.
				// However, local_vars contains these arguments. So, we must
				// subtract the number of arguments from the number
				// of all locals. Then, we will have the number
				// of local variables that are not already allocated arguments.
				v.push_scope(&f, local_vars);
				for _ in 0..(len - args.len()) {
					code.push(LBC::BC(Bytecodes::Push(Value::None)));
				}
			}
			LLIR::Push(v) => code.push(LBC::BC(Bytecodes::Push(v.clone()))),
			LLIR::Pop => code.push(LBC::BC(Bytecodes::Pop)),
			LLIR::DynPop => code.push(LBC::BC(Bytecodes::DynPop)),
			LLIR::Duplicate => code.push(LBC::BC(Bytecodes::Duplicate)),
			LLIR::MkProd(p) => code.push(LBC::BC(Bytecodes::MkProd(*p))),
			LLIR::DeconsProd => code.push(LBC::BC(Bytecodes::DeconsProd)),
			LLIR::MkVariant(tag) => code.push(LBC::BC(Bytecodes::MkVariant(*tag))),
			LLIR::DeconsVariant => code.push(LBC::BC(Bytecodes::DeconsVariant)),

			LLIR::Add => code.push(LBC::BC(Bytecodes::Add)),
			LLIR::Sub => code.push(LBC::BC(Bytecodes::Sub)),
			LLIR::Mul => code.push(LBC::BC(Bytecodes::Mul)),
			LLIR::Div => code.push(LBC::BC(Bytecodes::Div)),
			LLIR::Mod => code.push(LBC::BC(Bytecodes::Mod)),
			LLIR::Eq => code.push(LBC::BC(Bytecodes::Eq)),
			LLIR::Neq => code.push(LBC::BC(Bytecodes::Neq)),
			LLIR::Lt => code.push(LBC::BC(Bytecodes::Lt)),
			LLIR::Lte => code.push(LBC::BC(Bytecodes::Lte)),
			LLIR::Gt => code.push(LBC::BC(Bytecodes::Gt)),
			LLIR::Gte => code.push(LBC::BC(Bytecodes::Gte)),
			LLIR::And => code.push(LBC::BC(Bytecodes::And)),
			LLIR::Or => code.push(LBC::BC(Bytecodes::Or)),
			LLIR::RuntimeErr(s) => code.push(LBC::BC(Bytecodes::RuntimeErr(s.clone()))),
		}
		idx += 1;
	}

	// We also need a function-calling interface
	code.push(LBC::LabelDecl("##Function_Calling_Interface".to_string()));
	// We expect that the stack is set up as follows:
	// 1: Closure for function we want to call
	// 2: Arg 1
	// 3: Arg 2
	// ...
	// n (bottom): Number indicating how many arguments there are


	// The idea here is to make a small loop that continuously calls the
	// closure on top of the stack with an argument below it until we run
	// out of arguments. We then return.
	let loop_start = String::from("##Function_Calling_Interface_Loop_Start");
	let loop_end = String::from("##Function_Calling_Interface_Loop_End");
	code.push(LBC::LabelDecl(loop_start.clone()));
	// Get the loop counter
	code.push(LBC::BC(Bytecodes::GetLoc(0)));
	// Push a 0
	code.push(LBC::BC(Bytecodes::Push(Value::Num(0.0))));
	// If > 0, continue
	code.push(LBC::BC(Bytecodes::Gt));
	code.push(LBC::GotoIfFalse(loop_end.clone()));
	// Update the counter
	code.push(LBC::BC(Bytecodes::GetLoc(0)));
	code.push(LBC::BC(Bytecodes::Push(Value::Num(1.0))));
	code.push(LBC::BC(Bytecodes::Sub));
	code.push(LBC::BC(Bytecodes::SetLoc(0)));
	// Otherwise, call the function
	code.push(LBC::BC(Bytecodes::Call));
	// And jump back to the start of the loop
	code.push(LBC::GotoLabel(loop_start));
	code.push(LBC::LabelDecl(loop_end.clone()));
	code.push(LBC::BC(Bytecodes::Hlt));

	code
}

/// Find every function in the LLIR. For each function,
/// register environmental variables and create memory locations,
/// and register the function itself in the VarManager.
fn register_fns(llir: &mut Vec<LLIR>, v: &mut VarManager) {
	let mut idx = 0;
	while idx < llir.len() {
		match &llir[idx] {
			LLIR::FnDecl(f, args) => {
				// Get the captured arguments and register.
				let (_, captured) = process_vars(idx, llir, &args);
				v.register_fn(&f, captured);
			}
			_ => ()
		}
		idx += 1;
	}
}

/// Given LLIR and the position of the function, along with its arguments, return a tuple
/// containing the local arguments & addresses and the captured arguments & addresses
fn process_vars(mut pos: usize, llir: &Vec<LLIR>, args: &Vec<String>) -> (HashMap<String, u64>, HashMap<String, u64>) {
	// Keep track of where we can allocate new variables
	let mut local_addr = 0;
	let mut captured_addr = 0;
	let mut local_map = HashMap::new();
	let mut captured_map = HashMap::new();

	// Args are local, so they should be allocated as such.
	for arg in args {
		match local_map.insert(arg.clone(), local_addr) {
			Some(_) => panic!("Arg {arg} already in local map (compiler error)"),
			None => ()
		}
		local_addr += 1;
	}

	while pos < llir.len() {
		match &llir[pos] {
			// If we have a Load instruction and no corresponding
			// local variable, then we must be loading an environmental variable.
			LLIR::Load(name) => {
				if !local_map.contains_key(name) {
					match captured_map.get(name) {
						Some(_) => (),
						None => {
							captured_map.insert(name.clone(), captured_addr);
							captured_addr += 1;
						}
					}
				}
			}
			// Create a slot for a variable containing a pointer to the function we want
			LLIR::VarDecl(name) => {
				match local_map.insert(name.clone(), local_addr) {
					None => local_addr += 1,
					Some(_) => panic!("Name {name} already declared (compiler error)")
				}
			}
			// The function has returned.
			LLIR::Ret => return (local_map, captured_map),
			_ => ()
		}
		pos += 1;
	}
	panic!("Return expected (compiler error)")
}

/// Given LBC and a map from qualified names to unique names, compile into Bytecode and a map from qualified names to function addresses.
fn lbc_to_bc(lbc: Vec<LBC>, qual_to_unique: HashMap<String, String>) -> (Vec<Bytecodes>, HashMap<String, u64>) {
	// Create a map from every label to a position in the code.
	let map = make_label_map(&lbc, qual_to_unique);
	let mut code = vec![];

	for ins in lbc.into_iter() {
		match ins {
			// Most of the LBC is literally just regular bytecode.
			// This translates 1:1.
			LBC::BC(b) => code.push(b),
			LBC::LabelDecl(_) => code.push(Bytecodes::Pass),
			// Translate a closure into an address and the number of arguments desired
			LBC::Closure(args, name) => {
				let addr = map.get(&name).expect(&format!("Name {name} not in label map (compiler error)"));
				code.push(Bytecodes::MkClosure(args, *addr));
			}
			// Translate into a jump
			LBC::GotoLabel(name) => {
				let addr = map.get(&name).expect(&format!("Name {name} not in label map (compiler error)"));
				code.push(Bytecodes::Jmp(*addr));
			}
			// Translate into a jump if false
			LBC::GotoIfFalse(name) => {
				let addr = map.get(&name).expect(&format!("Name {name} not in label map (compiler error)"));
				code.push(Bytecodes::Jz(*addr))
			}
		}
	}

	(code, map)
}

/// Given LBC and a map from qualified names to unique names, create a map from labels/names to addresses
fn make_label_map(lbc: &Vec<LBC>, qual_to_unique: HashMap<String, String>) -> HashMap<String, u64> {
	let mut pos = 0;
	let mut map = HashMap::new();

	while pos < lbc.len() {
		match &lbc[pos] {
			// Declare the position where our label was created
			LBC::LabelDecl(l) => {
				match map.insert(l.clone(), pos as u64) {
					Some(_) => panic!("Label {} already in map (compiler error)", l),
					_ => ()
				}
			}
			LBC::GotoIfFalse(_)
			| LBC::GotoLabel(_)
			| LBC::Closure(..)
			| LBC::BC(_) => ()
		}
		pos += 1;
	}

	for (key, val) in &qual_to_unique {
		match map.get(val) {
			// If our qualified name has a valid jump target, specify it in the map.
			Some(addr) => match map.insert(key.clone(), *addr) {
				Some(_) => panic!("Name {} already in addr map (compiler error)", key),
				_ => ()
			},
			_ => panic!("Name {} not in addr map (compiler error)", val)
		}
	}

	map
}

