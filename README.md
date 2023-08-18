# The Lacrima Programming Language

Lacrima is a type-safe, functional scripting language hosted in Rust. Although it 
is still in an early alpha phase, the goal is to become a usable, modern, general purpose 
programming language. 

## Some Notable Features 
Lacrima has (or is developing) the features expected of a modern language: modules, 
namespaces, FFI, a standard library, etc. Lacrima's more unique features are mostly 
contained within the core language. Lacrima core is a statically typed superset of System F,
augmented with algebraic data types, record types, pattern matching, and let statements. Unlike the ML family, Lacrima core does not limit the programmer to let-polymorphism; instead, it is possible to create type-level abstractions, just as one would do with values. 

## A Quick Introduction

### Basic Syntax

#### Literal Values
- Numbers: `10, 5.0, 4.999`, etc.
- Booleans: `True, False`
- Tuples: `(val1, val2, ...)`, etc.
- Lambdas: `(arg1: Type, arg2: Type, ...): Type => ...`
- Unit: `()`

#### Operators 
- `+, -, /, *, %` for Numbers
- `&&, ||` for Booleans 
- `==, !=` for all Atomic types (everything above except Lambdas)


#### If Statement 
An `if` statement is of the form 
```
if ... then ... else ...
```
where `else` is mandatory. For example, 
```
if 5 + 10 == 15 
then True 
else False
```

Other than top level statements, everything in the language is evaluable. Thus, we are 
easily able to nest `if` statements, or any other statement we may choose. 

```
if True 
then if False 
     then if 10 == 11 
          then if ...
          else ...
     else ...
else...
```
Long chains of `if-else` can be a little difficult to read, though. Luckily, Lacrima features
`match` statements to make this better.

#### Match Statement 

Simple `match` statements are of the form 
```
match ... 
    | pattern1 => ...
    | pattern2 => ...
    ...
```

We'll go more into depth into patterns later. For now, know that we can match 
against both literals and identifiers. For example, 

```
match 10 
    | 11 => 1   // Does not match, because 10 != 11
    | x => x    // Matches and binds `x` to 10. Evaluates to `x`.
```

We are also able to add guards to our patterns. 
```
match 10 
    | x if x == 11 => 1     // Does not match, because x != 11
    | x => 2                // Matches, evaluates to 2
```

#### Let Statement 
A `let` statement is of the form 
```
let myIdent: Type = expr1
in expr2
```
We evaluate this by evaluating `expr1` and binding it to the variable `myIdent`. 
Then, we evaluate `expr2`, which may contain instances of our identifier. 

#### Declaring a Function

Function declarations happen at the top level scope.

```
// Abstract example 
fn fnName (arg1: Type, arg2: Type, ...): Type => ...

// Concrete Example 
fn add (a: Num, b: Num): Num => a + b
```

We can use named functions elsewhere in our code by combining `let` and `lambda`. 
For example, 

```
let add: Num -> Num -> Num 
    = (a: Num, b: Num): Num => a + b
in add 10 11
```

I know things are a little.. verbose at the minute. Once type inference is working, this 
will be cleaner.

#### Declaring a Data Type 

A basic data type is of the form 
```
data myDataType 
    | Constructor1 Num 
    | Constructor2 Bool
```
This is an algebraic data type, just like you might find in Haskell, OCaml, or Rust. 
We can call the constructors to return a variant of the data type. 
```
...
Constructor1 10     // Evaluates to myDataType
```
We could, for example, create a list of numbers as follows. 

```
data NumList 
    | Cons (Num, NumList)
    | Nil ()

... 
// Create a list like [10, 11, 12]
Cons (10, Cons (11, Cons (12, Nil ())))
```

## Types
Lacrima is statically typed, meaning that it will fail to compile if terms or expressions
are incompatible. The basic literals can be typed as follows.
```
Numbers: Num
Booleans: Bool
Tuples: (Arg1 type, Arg2 type, ...)
Functions: Arg1 type -> Arg2 type -> ... -> Return type
```

Algebraic Data Types are typed with the data type name, followed by any type arguments. 
```
List a b c = 
    | Cons1 a 
    | Cons2 b 
    | Cons3 c

// A list with a=Bool, b=Num, c=Bool would be typed as 
List Bool Num Bool
```

Note that at the minute, the front end does not support ADT type arguments. 
You could try, but you'd get an error like `type ∀x.(() -> (List x)) cannot be called`. 
The core language supports type arguments just fine, though, so if you'd like to poke around 
with that, feel free. The only real work that needs to be done is desugaring the input, 
so it shouldn't be too hard to implement. 

## Modules and Namespaces 
Each file is treated as its own module with its own namespace. The filesystem should 
be organized as follows.
```
| main.lac
| folder1
    | mod1.lac
    | mod2.lac
    | folder2
        | ...
    | ...
```

Namespaces are organized according to the directory structure. 
For example, `main.lac` could import the `mod1.lac` namespace with 

```
import folder1::mod1
```

Note that only exported symbols are available through namespace resolution. 
Exporting can only be done at the top level to `data` or `fn` items. 
For example, if `mod1.lac` contains the following code, 

```
export fn add(a: Num): Num => a + 100

export data MaybeNum 
    | Just Num
    | Nothing ()
```

then `main.lac` could use those definitions like so:

```
...
mod1::add 10

...
mod1::Just 11
```

The previous examples have all been relative namespace resolution. If absolute 
namespaces are required, we can start from the root namespace, `root`. For example, 
if `mod1` wanted to include code from `main`, it would have the statement 
```
import root::main
```

## FFI
Exported functions are available to call through the Rust FFI. For example, if our `main.lac`
file is as follows, 
```
export fn main(_: Num): Num => 10
```
then we can write the following Rust code to call the `main` function. 
```rust
vm.call("root::main::main", vec![Value::Num(0.0)]);
```
There is no system yet in place to typecheck these calls. It is up to the programmer 
to maintain correctness. 


We can call the other way, too. First, we register the function in Rust. 
```rust
vm.register_fn("print", |vals| {
    println!("{:?}, vals[0]");
    Value::None
});
```
In our Lacrima code, we must first include the function with the `extern` keyword. We 
can then call it. 

```
extern print: Num -> ()

...
print 10        // Prints `10` to stdout
```


## In Progress
As stated above, Lacrima is still in a very early state. Core functionality, like 
ADT arguments, still need to be implemented in the front end. Here is a rough timeline 
for the language. 

### Very important, near future goals
- [x] Create front-end that desugars to Core
- [x] FFI
- [x] Namespacing
- [ ] Desugar polymorphic functions and datatypes
- [ ] Atomic List type
- [ ] Compiler-aided testing (and bugfixing)
- [ ] Better (much better) error messages
- [ ] Type classes
- [ ] Bare-bones standard library
- [ ] Bare-bones editor tools (LSP, etc.)  

### Important, further future goals 
- [ ] Named product (record) types
- [ ] Auto Documentation
- [ ] Language Optimization
- [ ] Featurefull LSP, editor tools 
- [ ] Expanded standard library
- [ ] Other desugaring (list comprehension, name punning, etc)

### Distant goals 
- [ ] Higher kinded type system
- [ ] GADTs
- [ ] Compiler optimizations 
- [ ] JIT
- [ ] Non-standard libraries (HTTP, Graphics, Web, etc)
- [ ] ... More to come, I'm sure
