use super::{values::Value, bytecode::Bytecodes};

/// Provide a helpful debug print to show the stack, frame, sp, and nearby instructions.
pub fn debug_print(stack: &Vec<Value>, pc: &usize, fp: &(usize, usize), sp: &usize, code: &Vec<Bytecodes>) {
	print_stack(stack, fp, sp);
	println!();
	print_ins(code, pc);
	println!();
	println!("{}", "-".repeat(64));
	println!();
}

fn print_stack(stack: &Vec<Value>, fp: &(usize, usize), sp: &usize) {
	if fp.0 == 0 && *sp == 0 {
		println!("Empty Stack");
		return;
	}
	/*
		Item 1 | Item 2 | Item 3 | Item 4 ...
		FRAME^                     SP^
	*/
	let mut result = String::new();
	let mut frame_pos = 0;
	for i in 0..(fp.0 + *sp) {
		if i == fp.0 {
			frame_pos = result.len();
		}
		result += &format!("{:?} | ", stack[i]);
	}
	let info = format!("{}^FRAME", " ".repeat(frame_pos));

	println!("{}", result);
	println!("{}", info);
}

fn print_ins(code: &Vec<Bytecodes>, pc: &usize) {
	let start = if *pc as i32 - 3 < 0 { 0 } else { pc - 3 };
	let end = if pc + 3 >= code.len() { code.len() } else { pc + 3 };

	let mut result = String::new();
	let mut pc_pos = 0;
	for idx in start..end {
		if idx == *pc {
			pc_pos = result.len();
		}
		result += &format!("{:?}", &code[idx]);
		if idx != end - 1 {
			result += " | ";
		}
	}
	let info = format!("{}PC^", " ".repeat(pc_pos));
	println!("{}", result);
	println!("{}", info);
}
