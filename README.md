# ChillLang

A compiled, statically typed programming language with a self-contained compiler (`chc`) written in C. ChillLang compiles to native x86-64 machine code via Intel-syntax assembly.

## Overview

ChillLang is a C-like language with a cleaner syntax - variables are immutable by default, structs are first-class, and the full compiler pipeline (lexer → parser → semantic analysis → IR → SSA → codegen) is implemented from scratch in C11.

## Language Features

### Variables

Variables are immutable by default. Use `mut` to declare a mutable variable.

```
i32 x = 10;        // immutable
mut i32 y = 0;     // mutable
```

### Functions

```
fun add(i32 a, i32 b) i32 {
    return a + b;
}

fun main() i32 {
    mut i32 a = 3;
    a = add(a, 5);
    return a;
}
```

### Structs

Structs support nested fields, struct literals, and member access.

```
struct Point {
    x: i32,
    y: i32
}

fun create_point(i32 x, i32 y) Point {
    return { x: x, y: y };
}

fun main() i32 {
    Point p = create_point(5, 0);
    p.x = p.x + 1;   // requires p to be mutable
    return p.x;
}
```

### Control Flow

```
// if / else
if (x > 0) {
    // ...
} else {
    // ...
}

// while loop
while (x < 10) {
    x = x + 1;
}

// for loop
for (mut i32 i = 0; i < 5; i = i + 1) {
    // ...
}

// break / continue
for (mut i32 i = 0; i < 10; i = i + 1) {
    if (i == 5) { break; }
}
```

### Type System

| Type | Description |
|------|-------------|
| `i8`, `i16`, `i32`, `i64` | Signed integers |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers |
| `f32`, `f64` | Floating point |
| `bool` | Boolean (`true` / `false`) |
| `struct` | User-defined composite types |

Integer and float literals support explicit suffixes: `42i32`, `3.14f64`, etc.

### Operators

- **Arithmetic:** `+`, `-`, `*`, `/`, `%`
- **Bitwise:** `&`, `|`, `^`, `~`, `<<`, `>>`
- **Comparison:** `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Logical:** `&&` / `and`, `||` / `or`, `!` / `not`
- **Compound assignment:** `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`
- **Cast:** `expr as type`

## Compiler Architecture

```
Source (.chl)
    │
    ▼
 Lexer          — tokenizes source into a stream of tokens
    │
    ▼
 Parser         — recursive descent; builds an AST
    │
    ▼
 Semantic       — type checking, name resolution, struct layout
    │
    ▼
 IR Lowering    — AST → flat three-address IR with virtual registers
    │
    ▼
 SSA / Phi      — CFG construction, dominance analysis, phi insertion,
                  SSA renaming, phi elimination
    │
    ▼
 Optimizations  — dead code elimination, redundant branch elimination,
                  unused label pruning, dead alloca elimination
    │
    ▼
 x64 Codegen    — emits Intel-syntax x86-64 assembly
    │
    ▼
 gcc (linker)   — assembles and links the final executable
```

## Building

**Requirements:** CMake ≥ 3.10, a C11 compiler, and `gcc` (used at link time).

```sh
cmake -B build
cmake --build build
```

This produces the `chc` binary inside `build/`.

## Usage

```sh
chc <source.chl> -o <output>
```

**Example:**

```sh
chc test.chl -o test
./test
```

## Project Structure

```
ChillLang/
├── src/
│   ├── main.c            # Entry point, compiler driver
│   ├── lexer.c           # Tokenizer
│   ├── parser.c          # Recursive descent parser
│   ├── ast.c             # AST node definitions and dump
│   ├── semantic.c        # Semantic analysis and type checking
│   ├── type.c            # Type system and type registry
│   ├── symtab.c          # Symbol table
│   ├── ir.c              # IR data structures
│   ├── ir_builder.c      # IR construction helpers
│   ├── lower_ir.c        # AST → IR lowering
│   ├── ir_phi.c          # SSA construction (dominance, phi nodes)
│   ├── ir_opt.c          # Optimization passes
│   ├── x64_codegen.c     # x86-64 assembly emission
│   └── include/          # Header files
├── docs/
│   └── grammar.md        # Formal language grammar (BNF)
├── CMakeLists.txt
└── LICENSE               # GPL v3
```

## License

ChillLang is licensed under the [GNU General Public License v3.0](LICENSE).
