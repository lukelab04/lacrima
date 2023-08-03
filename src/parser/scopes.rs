use std::collections::HashMap;
use core_lang::errors::*;
use std::sync::atomic::AtomicUsize;
use crate::ast::Ident;
use crate::namespace::*;

/// A mutithread-safe way to get a unique identifier from a non-unique identifier.
pub fn gen_unique_name(id: &str) -> String {
	static COUNT: AtomicUsize = AtomicUsize::new(0);
	format!(
		"{}#{}",
		id,
		COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
	)
}


/// Manage scope for a specific namespace.
///
/// There are two main data structures here: the Scope Stack and the Resolver.
///
/// As the name suggests, the Scope Stack is a stack of scopes to keep track of local variables.
/// Lexical scope is handled how it would be in lambda calculus. That is, inner scopes
/// are allowed to shadow variable names from outer scopes. Each scope is a simply a hash map
/// from the local name to a unique name. We enter and exit scopes by pushing or popping scopes from the stack.
///
/// The Resolver maps unique names to information about that name. Lexical scope does not have
/// any bearing on the resolver; it is a flat map.
///
/// The primary purpose of this data structure is to provide an easy way to query information about
/// a variable based on its scope and local name by first getting the unique name from the Scope Stack
/// and then getting information from the Resolver.
pub struct ScopeManager {
	resolver: NameResolver,
	manager: NameSpaceManager,
	namespace: NameSpacePath,
	// local name -> unique name
	scopes: Vec<HashMap<String, String>>,
	// Unique data names -> Local constructor names for that data -> Unique constructor name for data
}

impl ScopeManager {
	pub fn new(namespace: NameSpacePath, manager: NameSpaceManager, resolver: NameResolver) -> ScopeManager {
		ScopeManager {
			manager,
			resolver,
			namespace,
			scopes: vec![HashMap::new()],
		}
	}
	pub fn get_namespace_path(&self) -> NameSpacePath { self.namespace.clone() }
	fn last_scope(&mut self) -> &mut HashMap<String, String> {
		self.scopes.last_mut().expect("No last scope (compiler error)")
	}
	pub fn enter_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}
	pub fn exit_scope(&mut self) {
		self.scopes.pop();
	}

	/// Register a standard identifier within the current scope, and add it to the Resolver.
	/// Additionally, mutate the identifier so that the name is unique.
	/// # Errors
	/// Returns an error if the name was already defined.
	pub fn register_standard_ident(&mut self, ident: &mut Ident) -> Res<()> {
		let new_name = gen_unique_name(&ident.name);
		match self.last_scope().insert(ident.name.clone(), new_name.clone()) {
			None => {
				ident.name = new_name;
				self.resolver.add_standard(&ident);
				Ok(())
			}
			Some(_) => Err(ErrMsg::new(&format!("Name {} already declared in this scope", ident.name)))
		}
	}
	/// Register a type variable within the current scope, and add it to the Resolver.
	/// Additionally, mutate the identifier so that the name is unique.
	/// # Errors
	/// Returns an error if the name was already defined.
	pub fn register_type_var(&mut self, ident: &mut Ident) -> Res<()> {
		let new_name = gen_unique_name(&ident.name);
		match self.last_scope().insert(ident.name.clone(), new_name.clone()) {
			None => {
				ident.name = new_name;
				self.resolver.add_type_var(&ident);
				Ok(())
			}
			Some(_) => Err(ErrMsg::new(&format!("Name {} already declared in this scope", ident.name)))
		}
	}
	/// Register a constructor within the current scope, and add it to the Resolver.
	/// Additionally, mutate the identifier so that the name is unique.
	/// # Errors
	/// Returns an error if the name was already defined.
	pub fn register_cons(&mut self, ident: &mut Ident, args: usize) -> Res<()> {
		let new_name = gen_unique_name(&ident.name);
		match self.last_scope().insert(ident.name.clone(), new_name.clone()) {
			None => {
				ident.name = new_name;
				self.resolver.add_cons(&ident, args);
				Ok(())
			}
			Some(_) => Err(ErrMsg::new(&format!("Name {} already declared in this scope", ident.name)))
		}
	}
	/// Register a data type within the current scope, and add it to the Resolver.
	/// Additionally, mutate the identifier so that the name is unique.
	/// # Errors
	/// Returns an error if the name was already defined.
	pub fn register_data(&mut self, ident: &mut Ident, args: usize) -> Res<()> {
		let new_name = gen_unique_name(&ident.name);
		match self.last_scope().insert(ident.name.clone(), new_name.clone()) {
			None => {
				ident.name = new_name;
				self.resolver.add_data(&ident, args);
				Ok(())
			}
			Some(_) => Err(ErrMsg::new(&format!("Name {} already declared in this scope", ident.name)))
		}
	}

	/// Register an identifier that was already exported. This adds the identifier to the local scope,
	/// but does *not* add it to the resolver. Because it is exported, it should already be in the resolver.
	pub fn register_exported_ident(&mut self, ident: &mut Ident) -> Res<()> {
		let new_name = self.manager.get_exported_name(&self.namespace, &ident.name)?;
		match self.last_scope().insert(ident.name.clone(), new_name.clone()) {
			None => {
				ident.name = new_name;
				Ok(())
			}
			Some(_) => Err(ErrMsg::new(&format!("Name {} already declared in this scope", ident.name)))
		}
	}
	/// Register an external identifier in the current scope and in the Resolver.
	/// The identifier is *not* mangled, and no unique name is created. This is
	/// so that no external functions may clash.
	pub fn register_extern_ident(&mut self, ident: &mut Ident) -> Res<()> {
		match self.last_scope().insert(ident.name.clone(), ident.name.clone()) {
			None => {
				self.resolver.add_standard(&ident);
				Ok(())
			},
			Some(_) => Err(ErrMsg::new(&format!("Name {} already declared in this scope", ident.name)))
		}
	}

	/// Given the local name of an identifier, get its unique name.
	fn get_unique_name(&self, name: &str) -> Res<String> {
		// Loop backwards through the scopes until we find what we are looking for, or fail.
		let mut idx = self.scopes.len();
		loop {
			if idx == 0 { break; }
			idx -= 1;
			match self.scopes[idx].get(name) {
				Some(n) => return Ok(n.clone()),
				_ => ()
			}
		}
		Err(ErrMsg::new(&format!("Identifier {} was not declared in this scope", name)))
	}

	/// Register a constructor with a specified data type.
	/// # Errors
	/// Returns an error if the constructor was already added.
	pub fn add_cons_to_data(&mut self, unique_data_name: &str, local_cons_name: &str) {
		let un = self.get_unique_name(local_cons_name).expect(&format!("Constructor {} not declared (compiler error)", local_cons_name));
		self.resolver.add_cons_to_data(unique_data_name, local_cons_name, &un);
	}

	/// Given a local identifier, ensure that it was declared and exists in the Resolver.
	/// Mutate the identifier to have a unique name.
	/// # Errors
	/// Returns an error if the identifier was not declared.
	pub fn resolve_local(&mut self, ident: &mut Ident) -> Res<NameEntry> {
		let name = self.get_unique_name(&ident.name)?;
		let entry = self.resolver.get_entry(&name);
		ident.name = name;
		Ok(entry)
	}

	/// Given a NameSpacePath, find the corresponding NameEntry.
	/// # Errors
	/// Return an error if the NameSpacePath is unreachable or does not point to an identifier.
	pub fn resolve_namespace(&mut self, n: &NameSpacePath) -> Res<NameEntry> {
		let pv = n.get_path_vec();
		let mut pos = 0;
		let mut curr_namespace = self.namespace.clone();

		// Each namespace has a list of visible namespaces from it. So,
		// we hop from namespace to namespace until we find what we are looking for.
		loop {
			if pos >= pv.len() - 1 { break; }
			curr_namespace = self.manager.get_reachable_namespace(&curr_namespace, &pv[pos])?;
			pos += 1;
		}

		let name = self.manager.get_exported_name(&curr_namespace, &pv[pos])?;
		Ok(self.resolver.get_entry(&name))
	}

	/// If the name supplied points to a constructor, return `Some` and the number of arguments.
	/// Otherwise, return `None`.
	pub fn is_constructor(&self, local_name: &str) -> Option<usize> {
		let name = match self.get_unique_name(local_name) {
			Ok(n) => n,
			_ => return None
		};
		match self.resolver.get_entry(&name) {
			NameEntry::Constructor { arguments, .. } => Some(arguments),
			_ => None
		}
	}
}

