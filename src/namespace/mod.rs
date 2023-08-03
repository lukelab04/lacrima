use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::RwLock;
use core_lang::errors::*;
use crate::ast::Ident;
use crate::lexer::{lex, Token, TokKind};

/// Provide a thread safe mapping from unique identifier names to information regarding those identifiers
#[derive(Clone)]
pub struct NameResolver {
	names: Rc<RwLock<HashMap<String, NameEntry>>>,
}

impl NameResolver {
	pub fn new() -> Self {
		Self {
			names: Rc::new(RwLock::new(HashMap::new()))
		}
	}
	/// Insert a key-value pair into the resolver
	/// # Panics
	/// Panics if the name already exists in the resolver
	fn insert(&mut self, name: &str, entry: NameEntry) {
		let mut names = self.names.write().expect("RW lock poisoned (compiler error)");
		match names.insert(name.to_string(), entry) {
			Some(_) => panic!("Identifier {} already declared (compiler error)", name),
			_ => ()
		}
	}
	/// Add a data declaration to the resolver
	pub fn add_data(&mut self, name: &Ident, args: usize) {
		self.insert(&name.name, NameEntry::Data { arguments: args, constructors: HashMap::new(), id: name.clone() })
	}
	/// Add a constructor declaration to the resolver
	pub fn add_cons(&mut self, name: &Ident, args: usize) {
		self.insert(&name.name, NameEntry::Constructor { arguments: args, id: name.clone() })
	}
	/// Add a type variable to the resolver
	pub fn add_type_var(&mut self, name: &Ident) {
		self.insert(&name.name, NameEntry::TypeVar { id: name.clone() })
	}
	/// Add a regular variable to the resolver
	pub fn add_standard(&mut self, name: &Ident) {
		self.insert(&name.name, NameEntry::Standard { id: name.clone() })
	}

	#[allow(unused)]
	/// Get the number of generic arguments associated with the unique name specified
	/// # Panics
	/// Panics if the name does not correspond with a `Data` entry
	pub fn get_data_args(&self, name: &str) -> usize {
		let names = self.names.read().expect("RW lock poisoned (compiler error)");
		match names.get(name) {
			Some(NameEntry::Data { arguments, .. }) => *arguments,
			_ => panic!("Identifier {} was not declared as 'data' (compiler error)", name)
		}
	}
	/// Register a constructor as belonging to a specific data type.
	/// # Panics
	/// Panics if the constructor was already added, or if the data name specified does not exist in the resolver
	pub fn add_cons_to_data(&mut self, data: &str, local_cons: &str, unique_cons: &str) {
		let mut names = self.names.write().expect("RW lock poisoned (compiler error)");
		match names.get_mut(data) {
			Some(NameEntry::Data { constructors, .. }) => match constructors.insert(local_cons.to_string(), unique_cons.to_string()) {
				Some(_) => panic!("Constructor {} already added to data (compiler error)", local_cons),
				_ => ()
			}
			_ => panic!("Identifier {} not declared as data (compiler error)", data)
		}
	}
	/// Get the NameEntry corresponding with a unique name
	/// # Panics
	/// Panics if the name does not exist in the resolver
	pub fn get_entry(&self, name: &str) -> NameEntry {
		let names = self.names.read().expect("RW lock poisoned (compiler error)");
		match names.get(name) {
			Some(e) => e.clone(),
			_ => panic!("Identifier {} not added to name resolver (compiler error)", name)
		}
	}
}

#[derive(Clone)]
pub enum NameEntry {
	Data {
		arguments: usize,
		// Local name -> unique name
		constructors: HashMap<String, String>,
		id: Ident,
	},
	Constructor {
		arguments: usize,
		id: Ident,
	},
	TypeVar {
		id: Ident,
	},
	Standard {
		id: Ident,
	},
}

/// Keep a flattened map of the entire project structure.
pub struct ProjectStructure {
	pub namespaces: NameSpaceManager,
	pub resolver: NameResolver,
	// (Fully qualified namespace, tokens for file)
	pub tokens: Vec<(NameSpacePath, Vec<Token>)>,
}

#[derive(Clone)]
/// A thread-safe manager for all namespaces in a project
pub struct NameSpaceManager {
	// fully qualified name -> namespace
	namespaces: Rc<RwLock<HashMap<NameSpacePath, NameSpace>>>,
}

impl NameSpaceManager {
	pub fn new() -> NameSpaceManager {
		NameSpaceManager {
			namespaces: Rc::new(RwLock::new(HashMap::new()))
		}
	}
	#[allow(unused)]
	/// Pretty-print the namespaces of the project
	pub fn print(&self) {
		let map = self.namespaces.read().expect("RW Lock poisoned (compiler error)");
		for pair in map.iter() {
			println!("{}", pair.0.to_str());
			println!("Exported identifiers:");
			for item in &pair.1.idents {
				println!("\t{} => {}", item.0, item.1)
			}
			println!("Visible Namespaces:");
			for item in &pair.1.available_namespaces {
				println!("\t{} => {}", item.0, item.1.to_str())
			}
			println!("\n");
		}
	}
	/// Add a namespace to the project.
	/// # Errors
	/// Returns an error if the namespace was already added.
	pub fn add_namespace(&self, fully_qualified_name: NameSpacePath) -> Res<()> {
		let ns = NameSpace {
			idents: HashMap::new(),
			available_namespaces: HashMap::new(),
		};
		let mut map = self.namespaces.write().expect("RW Lock poisoned (compiler error)");
		match map.insert(fully_qualified_name.clone(), ns) {
			Some(_) => Err(ErrMsg::new(&format!("Namespace {} was already created", fully_qualified_name.to_str()))),
			_ => Ok(())
		}
	}
	/// Register an identifier as being exported from its namespace.
	/// # Errors
	/// Returns an error if the identifier was already exported, or if the specified namespace does not exist.
	pub fn add_exported_ident(&self, namespace: &NameSpacePath, local_name: &str, unique_name: &str) -> Res<()> {
		let mut map = self.namespaces.write().expect("RW Lock poisoned (compiler error)");
		match map.get_mut(namespace) {
			Some(m) => match m.idents.insert(local_name.to_string(), unique_name.to_string()) {
				Some(_) => Err(ErrMsg::new(&format!("Identifier {} was already exported", local_name))),
				_ => Ok(())
			}
			_ => panic!("Namespace {} has no corresponding map entry (compiler error)", namespace.to_str())
		}
	}
	/// Register a namespace as being visible from the current namespace, accessible from some local name.
	/// # Errors
	/// Returns an error if the namespace was already declared as visible, or if the current namespace does not exist
	pub fn add_visible_namespace(&self, curr_namespace: &NameSpacePath, being_added: &NameSpacePath, local_name: &str) -> Res<()> {
		let mut map = self.namespaces.write().expect("RW Lock poisoned (compiler error)");
		match map.get_mut(curr_namespace) {
			Some(n) => match n.available_namespaces.insert(local_name.to_string(), being_added.clone()) {
				Some(_) => Err(ErrMsg::new(&format!("Namespace {} was already imported", being_added.to_str()))),
				_ => Ok(())
			}
			_ => panic!("Cannot import namespace {} (compiler error)", curr_namespace.to_str())
		}
	}
	/// Request the unique name of some exported variable.
	/// # Errors
	/// Returns an error if the name specified is not in the namespace specified, or if the namespace does not exist.
	pub fn get_exported_name(&self, curr_namespace: &NameSpacePath, name: &str) -> Res<String> {
		let map = self.namespaces.read().expect("RW Lock poisoned (compiler error)");
		match map.get(curr_namespace) {
			Some(n) => match n.idents.get(name) {
				Some(n) => Ok(n.clone()),
				_ => Err(ErrMsg::new(&format!("Name {} not in namespace {}", name, curr_namespace.to_str())))
			}
			_ => Err(ErrMsg::new(&format!("Namespace {} does not exist", curr_namespace.to_str())))
		}
	}

	/// From the current namespace, fetch another namespace using its non-qualified name.
	/// # Errors
	/// Returns an error if the requested namespace is not reachable, or if the current namespace does not exist.
	pub fn get_reachable_namespace(&self, curr_namespace: &NameSpacePath, goal: &str) -> Res<NameSpacePath> {
		let map = self.namespaces.read().expect("RW Lock poisoned (compiler error)");
		match map.get(curr_namespace) {
			Some(n) => match n.available_namespaces.get(goal) {
				Some(g) => Ok(g.clone()),
				_ => Err(ErrMsg::new(&format!("Namespace {} is not reachable from {}", goal, curr_namespace.to_str())))
			}
			_ => panic!("Namespace {} not in map (compiler error)", curr_namespace.to_str())
		}
	}
}

/// A mapping of local identifier names to their unique names, and of available namespaces to their fully qualified names.
struct NameSpace {
	// Exported Local name -> unique name
	pub idents: HashMap<String, String>,
	pub available_namespaces: HashMap<String, NameSpacePath>
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct NameSpacePath {
	path: Vec<String>,
}

impl NameSpacePath {
	/// Append a path to the end of the current path.
	/// # Examples
	/// ```
	/// let mut ns = NameSpacePath::from_str("root::a::b");
	/// ns.append("c");
	/// assert_eq!(&ns.to_str(), "root::a::b::c")`
	/// ```
	pub fn append(&mut self, s: &str) {
		self.path.push(s.to_string())
	}
	/// Get the internal representation of a path
	pub fn get_path_vec(&self) -> Vec<String> { self.path.clone() }
	/// Convert a path to its string version
	/// # Panics
	/// Panics if the path is empty
	pub fn to_str(&self) -> String {
		if self.path.is_empty() {
			panic!("Empty namespace path (compiler error)");
		}
		let mut result = self.path[0].clone();
		for i in 1..self.path.len() {
			if i == self.path.len() - 1 {
				return result + "::" + &self.path[i]
			}
			result += &format!("::{}", &self.path[i]);
		}
		result
	}
	/// Get the last section of a path.
	/// # Panics
	/// Panics if the path is empty.
	/// # Examples
	/// ```
	/// let ns = NameSpacePath::from_str("root::a");
	/// assert_eq!(&ns.last_item(), "a");
	/// ```
	pub fn last_item(&self) -> String {
		if self.path.is_empty() { panic!("'last_item' called on empty namespace path (compiler error)") }
		self.path[self.path.len() - 1].clone()
	}
	/// Convert a string into a NameSpacePath.
	/// Internally, this splits the string at each `"::"` sequence.
	/// # Examples
	/// ```
	/// let ns = NameSpacePath::from_str("a::b");
	/// // ns is a vector of ["a", "b"]
	/// ```
	pub fn from_str(s: &str) -> NameSpacePath {
		Self {
			path: s.split("::").map(|x| x.to_string()).collect()
		}
	}
	/// From the current namespace, get the relative namespace from a string `s`.
	/// Relative here does not mean the same thing it would with directory paths.
	/// Instead, we are searching the current directory for the relative namespace.
	/// If the relative namespace starts with 'root', we simply convert that to a namespace and return.
	/// # Panics
	/// * Panics if the current namespace does not begin with 'root'.
	/// * Panics if the current namespace is equal to 'root'.
	/// # Examples
	/// ```
	/// let n1 = NameSpacePath::from_str("root::a::b");
	/// // We calcuate relative to the current directory, "b".
	/// assert_eq!(&n1.relative("c::d").to_str(), "root::a::c::d");
	/// // "root::a::b::root::x" doesn't make sense, so we default to "root::x".
	/// assert_eq(&n1.relative("root::x").to_str(), "root::x");
	/// ```
	pub fn relative(&self, s: &str) -> NameSpacePath {
		if s.starts_with("root") {
			return NameSpacePath::from_str(s)
		}

		if self.path.is_empty() || self.path[0] != "root" {
			panic!("Namespace does not start with 'root' (compiler error)")
		}
		if self.path.len() == 1 {
			panic!("Cannot calculate relative namespace of 'root' (compiler error)")
		}

		let mut path = self.path[0..self.path.len() - 1].to_vec();
		path.extend(s.split("::").map(|x| x.to_string()));
		Self {
			path
		}
	}
	/// Convert a namespace to a path, given the root path.
	/// NameSpacePaths are similar to regular file paths, where all but the last
	/// 'section' designate folders. For example, the namespace `"root::a::b"` would
	/// refer to the file `'b.lac'` in the directory structure `'root/a/b.lac'`.
	/// Using this information and a root path, we can calculate a path to our namespace.
	/// # Panics
	/// Panics if the current namespace does not start with 'root'.
	pub fn to_path(&self, root_path: &Path) -> PathBuf {
		if self.path.is_empty() || self.path[0] != "root" {
			panic!("Namespace path does not start with root (compiler error)");
		}
		let mut p = root_path.to_path_buf();
		for i in 1..self.path.len() {
			if i == self.path.len() - 1 {
				return p.join(self.path[i].clone() + ".lac")
			}
			p = p.join(&self.path[i]);
		}
		p
	}
}

/// Given a path to the root file of the project, construct all namespaces and import all necessary files.
/// Circular dependencies are not an issue. We keep track of which files were visited, and do not visit twice.
pub fn build_project_structure(source: &Path) -> Res<ProjectStructure> {
	// We should ensure that the source actually points to a file
	if !source.is_file() {
		return Err(ErrMsg::new(&format!("Path {:?} is not a file.", source)))
	}
	// We should also ensure we are compiling a .lac file
	if source.extension().is_none() || source.extension().unwrap() != "lac" {
		return Err(ErrMsg::new(&format!("File {:?} has an unknown extension; please use '.lac'", source)))
	}

	let root_name = source
		.file_stem()
		.expect(&format!("Could not get name for file {:?}", source))
		.to_str()
		.expect(&format!("Could not convert file {:?} to valid utf-8", source));

	let root_path = source.parent().expect(&format!("Couldn't get root path for {:?}", source));
	// List of namespaces left to include in the project
	let mut to_search: Vec<NameSpacePath> = vec![NameSpacePath::from_str(&format!("root::{}", root_name))];
	// List of namespaces we have already included
	let mut already_searched = HashSet::new();
	let nm = NameSpaceManager::new();
	let mut resolver = NameResolver::new();
	// Pairing of tokens with their namespaces
	let mut all_toks = vec![];

	while let Some(path) = to_search.pop() {
		// If already searched, return
		match already_searched.insert(path.clone()) {
			false => continue,
			_ => ()
		}
		let file_path = path.to_path(&root_path);
		if !file_path.exists() {
			return Err(ErrMsg::new(&format!("Cannot import '{}' because file {:?} does not exist", path.to_str(), file_path)))
		}

		let toks = lex(&std::fs::read_to_string(file_path.clone()).expect(&format!("Could not read {:?}", file_path)))?;

		// Create a namespace from the current namespace path
		nm.add_namespace(path.clone())?;

		let mut place = 0;
		while place < toks.len() {
			// Import statements are of the form 'import x::y::...::z'. We want to parse this namespace,
			// convert it to a path, and add it to the list of things to include.
			if toks[place].kind == TokKind::Import {
				place += 1;
				let mut import_str = String::new();
				// Parse the namespace
				while place < toks.len() && toks[place].kind == TokKind::Ident {
					import_str += &toks[place].lexeme.clone();
					place += 1;
					if place < toks.len() && toks[place].kind == TokKind::DoubleColon {
						place += 1;
						import_str += "::";
					}
				}
				// If we can't parse anything, that's bad.
				if import_str.is_empty() {
					return Err(ErrMsg::new("Expected identifier after 'import'"))
				}
				// It is possible that we are specifying a relative path (like 'import neighbor_file::identifier').
				// So we should resolve that, then ensure that this new imported namespace is visible from the current namespace.
				let rel = path.relative(&import_str);
				nm.add_visible_namespace(&path, &rel, &rel.last_item())?;
				to_search.push(rel);
				continue;
			}
			// We could be exporting a Data type or a function. They are each handled differently.
			if toks[place].kind == TokKind::Export {
				place += 1;
				if place >= toks.len() { return Err(ErrMsg::new("Expected 'data' or 'fn' after 'export' ")) }
				match toks[place].kind {
					// We need to register the data type and all of its constructors in the current namespace. This means
					// they must also be given unique names.
					TokKind::Data => {
						place += 1;
						if place >= toks.len() || toks[place].kind != TokKind::Ident {
							return Err(ErrMsg::new("Expected identifier after 'data'"));
						}
						// Save the location of the Data type identifier
						let data_loc = place;
						place += 1;

						// After 'data ident', we can specify any number of type arguments.
						// These should all be single identifiers, so we can easily
						// parse through and count them.
						let mut arg_count = 0;
						while place < toks.len() && toks[place].kind == TokKind::Ident {
							arg_count += 1;
							place += 1;
						}

						// Export our new data name and add to resolver
						let unique_data_name = crate::parser::scopes::gen_unique_name(&toks[data_loc].lexeme);
						nm.add_exported_ident(&path, &toks[data_loc].lexeme, &unique_data_name)?;
						let id = Ident {
							token: toks[data_loc].clone(),
							name: unique_data_name.clone()
						};
						resolver.add_data(&id, arg_count);

						// We also need to export all the constructors. Parsing this is a little trickier,
						// because we need to skip forward arbitrarily far before *possibly* hitting the
						// next constructor. We can resolve this by immediately breaking the loop if any top-level
						// keywords appear that can't show up in a data declaration.
						// Other than that, things are pretty easy -- we just export our constructors and
						// add them to the resolver.
						while place < toks.len() {
							let kind = &toks[place].kind;
							if *kind == TokKind::Data || *kind == TokKind::Fn || *kind == TokKind::Export || *kind == TokKind::Import { break; }
							if *kind == TokKind::Bar {
								place += 1;
								if place > toks.len() || toks[place].kind != TokKind::Ident {
									return Err(ErrMsg::new("Expected identifier"));
								}
								// Generate a new name and add to resolver
								let unique_cons_name = crate::parser::scopes::gen_unique_name(&toks[place].lexeme);
								resolver.add_cons_to_data(&unique_data_name, &toks[place].lexeme, &unique_cons_name);
								let id = Ident {
									token: toks[place].clone(),
									name: unique_cons_name.clone()
								};
								resolver.add_cons(&id, arg_count);
								// Export
								nm.add_exported_ident(&path, &toks[place].lexeme, &unique_cons_name)?;
							} else {
								place += 1;
							}
						}

						continue;
					},
					// To export a function, we just need its name. So we parse the name,
					// generate a unique ID, export it, and add it to the resolver.
					TokKind::Fn => {
						place += 1;
						if place >= toks.len() || toks[place].kind != TokKind::Ident {
							return Err(ErrMsg::new("Expected identifier after 'fn'"));
						}
						let unique_name = crate::parser::scopes::gen_unique_name(&toks[place].lexeme);
						nm.add_exported_ident(&path, &toks[place].lexeme, &unique_name)?;
						let id = Ident {
							token: toks[place].clone(),
							name: unique_name.clone()
						};
						resolver.add_standard(&id);
					},
					_ => return Err(ErrMsg::new("Expected 'data' or 'fn' after 'export' ")),
				}
			}
			place += 1;
		}
		all_toks.push((path, toks));
	}

	Ok(ProjectStructure {
		resolver,
		tokens: all_toks,
		namespaces: nm
	})
}