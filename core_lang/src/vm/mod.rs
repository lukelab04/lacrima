use std::collections::HashMap;

use self::{values::*, bytecode::Bytecodes};

pub mod bytecode;
pub mod values;
mod debugger;

const STACK: usize = 4096;
const DEBUG: bool = false;

/// A Virtual Machine to execute Bytecode.
pub struct Vm {
	registered_functions: HashMap<String, Box<dyn Fn(Vec<Value>) -> Value>>,
	fn_name_to_addr: HashMap<String, u64>,
	code: Vec<Bytecodes>,
	stack: Vec<Value>,
	sp: usize,
	// Frame location on stack, Return address
	fp: Vec<(usize, usize)>,
	environments: Vec<Vec<Value>>,
}

impl Vm {
	pub fn new(code: Vec<Bytecodes>, fn_name_to_addr: HashMap<String, u64>) -> Vm {
		Vm {
			registered_functions: HashMap::new(),
			stack: vec![Value::None; STACK],
			sp: 0,
			fp: vec![(0, 0)],
			environments: vec![],
			code,
			fn_name_to_addr,
		}
	}
	/// Register a function pointer with a name for future use.
	pub fn register_fn(&mut self, name: &str, f: fn(Vec<Value>) -> Value) {
		match self.registered_functions.insert(name.to_string(), Box::new(f)) {
			Some(_) => panic!("Function {} was already registered", name),
			_ => ()
		}
	}
	/// Get the top frame pointer
	fn fp(&self) -> usize { self.fp.last().expect("No last fp (vm bug)").0 }
	/// Push an item to the stack
	pub fn push(&mut self, item: Value) {
		let f = self.fp();
		self.stack[f + self.sp] = item;
		self.sp += 1;
	}
	/// Pop an item from the stack
	pub fn pop(&mut self) -> &Value {
		self.sp -= 1;
		let f = self.fp();
		&self.stack[f + self.sp]
	}
	/// Get an item at position `i` from the stack; calculate based on fp offset.
	pub fn get(&self, i: usize) -> Value {
		self.stack[self.fp() + i].clone()
	}
	/// Set an item at position `i` on the stack; calculate based on fp offset.
	pub fn set(&mut self, i: usize, val: Value) {
		let f = self.fp();
		self.stack[f + i] = val;
	}
	/// Get the top environment.
	pub fn get_env(&self, i: usize) -> Value {
		self.environments.last().expect("No environment (vm bug)")[i].clone()
	}
	/// Set a new environment.
	pub fn set_env(&mut self, i: usize, v: Value) {
		self.environments.last_mut().expect("No environment (vm bug)")[i] = v;
	}
	/// Call a function with the specified arguments.
	pub fn call(&mut self, fn_name: &str, args: Vec<Value>) {

		// The big idea here is to get the address of the function we want to call,
		// put it into a closure, then pass that information (along with a counter variable
		// to tell how many arguments we have) to the Function Calling Interface.
		// The bus mostly drives itself after that.

		let addr = match self.fn_name_to_addr.get(fn_name) {
			Some(addr) => *addr,
			_ => panic!("Function {fn_name} not found in map")
		};
		let fn_caller = match self.fn_name_to_addr.get("##Function_Calling_Interface") {
			Some(fn_caller_addr) => *fn_caller_addr,
			_ => panic!("Function calling interface was not defined (compiler error)")
		};
		// First, we push the counter variable
		self.push(Value::Num(args.len() as f64));
		// Next, we push all the args, from back to front
		for arg in args.into_iter().rev() {
			self.push(arg);
		}
		// Then, we push a closure representing the function we're calling
		self.push(Value::Closure(Box::new(Closure {
			env: vec![],
			func: addr
		})));
		// Finally, we go to our function caller.
		self.exec(fn_caller as usize);
	}
	/// Start execution from a specified PC.
	fn exec(&mut self, place: usize) {
		use Bytecodes::*;
		let mut pc = place;

		macro_rules! num_op {
			($op: tt, $ret: expr) => {
				{
					let rhs = self.pop().clone();
					let lhs = self.pop().clone();
					match (lhs, rhs) {
						(Value::Num(a), Value::Num(b)) => self.push($ret(a $op b)),
						(a, b) => panic!("Cannot perform operation on non-numbers {a:?} and {b:?}(typecheck failure)")
					}
				}
			};
		}

		macro_rules! bool_op {
			($op: tt) => {
				{
					let rhs = self.pop().clone();
					let lhs = self.pop().clone();
					match (lhs, rhs) {
						(Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a $op b)),
						_ => panic!("Cannot perform operation on non-bools (typecheck failure)")
					}
				}
			};
		}

		macro_rules! eq_op {
			($op: tt) => {
				{
					let rhs = self.pop().clone();
					let lhs = self.pop().clone();
					match (lhs, rhs) {
						(Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a $op b)),
						(Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a $op b)),
						(Value::None, Value::None) => self.push(Value::Bool(true)),
						_ => panic!("Cannot perform operation (typecheck failure)")
					}
				}
			};
		}

		// Pretty straightforward switch-case bytecode interpreter.
		// Nothing here should be confusing, save for maybe the Call or Return instructions.
		loop {
			if DEBUG {
				let mut buf = String::new();
				debugger::debug_print(&self.stack, &pc, &self.fp.last().unwrap(), &self.sp, &self.code);
				std::io::stdin().read_line(&mut buf).unwrap();
			}

			match self.code[pc].clone() {
				Push(v) => self.push(v.clone()),
				DynPop => {
					let num = match self.pop() {
						Value::Num(n) => n,
						_ => panic!("DynPop with non-number (compiler error)")
					};
					for _ in 0..*num as u64 { self.pop(); }
				}
				Duplicate => {
					let val = self.get(self.sp - 1);
					self.push(val);
				}
				Pop => { self.pop(); },
				GetLoc(i) => self.push(self.get(i as usize)),
				SetLoc(i) => {
					let val = self.pop().clone();
					self.set(i.clone() as usize, val)
				}
				GetEnv(i) => self.push(self.get_env(i as usize)),
				SetEnv(i) => {
					let val = self.pop().clone();
					self.set_env(i as usize, val);
				}
				Jmp(i) => {
					pc = i as usize;
					continue
				},
				Jz(i) => if let Value::Bool(false) = self.pop() {
					pc = i as usize;
					continue
				},
				Jnz(i) => if let Value::Bool(true) = self.pop() {
					pc = i as usize;
					continue
				},
				Call => {
					// We want to construct a closure from the stuff on the stack,
					// push the new environment, update the fp and sp, then call the function.
					let closure = match self.pop() {
						Value::Closure(c) => c.clone(),
						_ => panic!("Cannot call non-closure (this is a compiler bug)")
					};
					self.environments.push(closure.env);
					let f = self.fp();
					// FP is set to the current top of stack (the argument to the function)
					self.fp.push((f + self.sp - 1, pc + 1));
					// SP is set to 1 (there is already something on the stack in this frame,
					// and the SP points to the first empty spot, so it cannot be 0).
					self.sp = 1;
					pc = closure.func as usize;
					continue;
				}
				CallExtern(name, args) => {
					// We pop the arguments off the stack, pass them to the Rust function, then
					// push the result back.
					let mut buf = vec![];
					for _ in 0..args {
						buf.push(self.pop().clone());
					}
					let f = match self.registered_functions.get(&name) {
						Some(f) => f,
						_ => panic!("Function {name} was not registered")
					};
					self.push(f(buf))
				},
				Ret => {
					// If there is no environment, then we must have called the function from Rust.
					// So instead of returning, we just stop running this function.
					if self.environments.is_empty() {
						return;
					}
					// Otherwise, we want to reset our FP and SP.
					self.environments.pop();
					// Get the top of the stack
					let return_val = self.pop().clone();
					let (old_fp, ret_addr) = self.fp.pop().expect("Frame ptr empty (vm error)");
					self.sp = old_fp - self.fp();
					// Push return value back on the stack
					self.push(return_val);
					pc = ret_addr;
					continue;
				}
				MkNum(n) => self.push(Value::Num(n)),
				MkBool(b) => self.push(Value::Bool(b)),
				MkProd(i) => {
					// Make a product from 'i' arguments on the stack.
					let mut vals = vec![];
					for _ in 0..i {
						vals.push(self.pop().clone());
					}
					self.push(Value::Prod(Box::new(vals)));
				}
				DeconsProd => {
					// Deconstruct a product by pushing its individual values onto the stack.
					let top = match self.pop().clone() {
						Value::Prod(p) => p,
						_ => panic!("Deconstruct product on non product (compiler error)")
					};
					for item in top.into_iter().rev() {
						self.push(item);
					}
				}
				MkVariant(tag) => {
					// Make a variant from a tag and data on the stack.
					let data = self.pop().clone();
					self.push(Value::Variant { tag, data: Box::new(data) })
				}
				DeconsVariant => {
					// Deconstruct a variant by pushing its data and tag onto the stack.
					let (tag, data) = match self.pop().clone() {
						Value::Variant { tag, data } => (tag, data),
						x => panic!("Deconstruct variant on {:?} (compiler error)", x)
					};

					self.push(*data);
					self.push(Value::Num(tag as f64));
				}
				MkClosure(i, c) => {
					// Make a closure from 'i' arguments and a function pointer 'c'
					let mut env = vec![];
					for _ in 0..i {
						env.push(self.pop().clone());
					}
					self.push(Value::Closure(Box::new(Closure {
						env,
						func: c
					})));
				}

				Add => num_op!(+, Value::Num),
				Sub => num_op!(-, Value::Num),
				Mul => num_op!(*, Value::Num),
				Div => num_op!(/, Value::Num),
				Mod => num_op!(%, Value::Num),
				Gt => num_op!(>, Value::Bool),
				Gte => num_op!(>=, Value::Bool),
				Lt => num_op!(<, Value::Bool),
				Lte => num_op!(<=, Value::Bool),

				Eq => eq_op!(==),
				Neq => eq_op!(!=),

				And => bool_op!(&&),
				Or => bool_op!(||),

				Pass => (),
				RuntimeErr(s) => {
					println!("Runtime Error: {}", s);
					break;
				}
				Hlt => break,
			}
			pc += 1;
		}
	}
}



