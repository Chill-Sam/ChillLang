# ChillLang Grammar

This document specifies the grammar of ChillLang using Extended Backus-Naur Form (EBNF).

**Notation:**
- `::=` defines a production rule; `;` ends it
- `|` separates alternatives
- `{ x }` - zero or more repetitions of `x`
- `[ x ]` - `x` is optional (zero or one occurrence)
- `( x | y )` - grouping of alternatives
- `"text"` - literal terminal token

---

## Lexical Grammar

### Whitespace and Comments

Whitespace (spaces, tabs, carriage returns, newlines) is insignificant and ignored between tokens.

Two comment forms are supported:

```
// line comment - extends to the end of the line

/* block comment - may span multiple lines */
```

### Identifiers

```
<letter>     ::= "A" | ... | "Z" | "a" | ... | "z" | "_" ;
<digit>      ::= "0" | ... | "9" ;
<identifier> ::= <letter> { <letter> | <digit> } ;
```

An identifier that matches a keyword is a keyword, not an identifier.

### Keywords

```
and   as   break   continue   else   false   for   fun
if    mut  not     or         return struct  true  while
```

> **Note:** `not`, `and`, and `or` are keyword aliases for `!`, `&&`, and `||` respectively.
> `not` is reserved but not currently usable as a prefix operator in expressions.

### Integer Literals

```
<oct_digit>       ::= "0" | ... | "7" ;
<hex_digit>       ::= <digit> | "a" | ... | "f" | "A" | ... | "F" ;
<int_suffix>      ::= ( "i" | "u" ) ( "8" | "16" | "32" | "64" ) ;

<dec_literal>     ::= <digit> { <digit> } [ <int_suffix> ] ;
<hex_literal>     ::= "0" ( "x" | "X" ) <hex_digit> { <hex_digit> } [ <int_suffix> ] ;
<bin_literal>     ::= "0" ( "b" | "B" ) ( "0" | "1" ) { "0" | "1" } [ <int_suffix> ] ;
<oct_literal>     ::= "0" ( "o" | "O" ) <oct_digit> { <oct_digit> } [ <int_suffix> ] ;

<integer_literal> ::= <dec_literal> | <hex_literal> | <bin_literal> | <oct_literal> ;
```

Examples: `42`, `255u8`, `0xFF`, `0b1010i32`, `0o17u16`

### Float Literals

```
<float_suffix>  ::= ( "f" | "F" ) ( "32" | "64" ) ;
<float_literal> ::= <digit> { <digit> } "." <digit> { <digit> } [ <float_suffix> ] ;
```

Float literals are decimal only (no hex floats). Examples: `3.14`, `1.0f32`, `2.718f64`

### Character and String Literals

```
<escape_seq>    ::= "\" <any_char> ;
<char_literal>  ::= "'" ( <escape_seq> | <any_char_except_"'"> ) "'" ;
<string_literal>::= '"' { <escape_seq> | <any_char_except_'"'> } '"' ;
```

### Boolean Literals

```
<bool_literal> ::= "true" | "false" ;
```

### Operators and Punctuation

```
Arithmetic:  +   -   *   /   %
Bitwise:     &   |   ^   ~   <<   >>
Logical:     !   &&   ||
Comparison:  ==   !=   <   >   <=   >=
Assignment:  =   +=   -=   *=   /=   %=   &=   |=   ^=
Delimiters:  (   )   {   }   [   ]
Other:       ,   ;   :   .   ->
```

---

## Syntactic Grammar

### Program

A program is a sequence of top-level declarations.

```
<program>        ::= { <top_level_item> } ;
<top_level_item> ::= <func_decl> | <struct_decl> ;
```

### Struct Declarations

```
<struct_decl> ::= "struct" <identifier> "{" [ <field_list> ] "}" ;
<field_list>  ::= <field> { "," <field> } [ "," ] ;
<field>       ::= <identifier> ":" <type> ;
```

Trailing commas after the last field are permitted.

Example:
```
struct Point {
    x: i32,
    y: i32,
}
```

### Function Declarations

```
<func_decl>  ::= "fun" <identifier> "(" [ <param_list> ] ")" [ <type> ] <block> ;
<param_list> ::= <param> { "," <param> } ;
<param>      ::= [ "mut" ] <type> <identifier> ;
```

If the return type is omitted, the function implicitly returns `void`.

Parameters declared with `mut` are mutable within the function body.

Example:
```
fun add(i32 a, i32 b) i32 {
    return a + b;
}
```

### Types

```
<type> ::= <identifier> ;
```

Built-in primitive types:

| Type    | Description                  |
|---------|------------------------------|
| `void`  | No value (return type only)  |
| `bool`  | Boolean                      |
| `i8`    | Signed 8-bit integer         |
| `u8`    | Unsigned 8-bit integer       |
| `i16`   | Signed 16-bit integer        |
| `u16`   | Unsigned 16-bit integer      |
| `i32`   | Signed 32-bit integer        |
| `u32`   | Unsigned 32-bit integer      |
| `i64`   | Signed 64-bit integer        |
| `u64`   | Unsigned 64-bit integer      |
| `f32`   | 32-bit floating-point        |
| `f64`   | 64-bit floating-point        |

User-defined struct names are also valid types.

### Blocks and Statements

```
<block> ::= "{" { <statement> } "}" ;

<statement> ::=
      <var_decl>    ";"
    | <return_stmt> ";"
    | <break_stmt>  ";"
    | <cont_stmt>   ";"
    | <expr_stmt>   ";"
    | <if_stmt>
    | <while_stmt>
    | <for_stmt>
    | <block>
    ;
```

`if`, `while`, `for`, and nested blocks do not require a trailing `;`.

#### Variable Declarations

```
<var_decl> ::= [ "mut" ] <type> <identifier> [ "=" <expression> ] ;
```

Variables without `mut` are immutable after initialization. The initializer is optional for `mut` variables.

Examples:
```
i32 x = 10;
mut i32 counter = 0;
mut Point p;
```

#### Return Statement

```
<return_stmt> ::= "return" [ <expression> ] ;
```

A bare `return;` with no expression returns from a `void` function.

#### Break and Continue

```
<break_stmt> ::= "break" ;
<cont_stmt>  ::= "continue" ;
```

Valid only inside `while` or `for` loop bodies.

#### Expression Statement

```
<expr_stmt> ::= <expression> ;
```

#### If Statement

```
<if_stmt> ::= "if" <expression> <block> [ "else" ( <if_stmt> | <block> ) ] ;
```

The condition is not parenthesized. The `else` branch may be another `if` for chaining.

Example:
```
if x > 0 {
    return x;
} else if x < 0 {
    return -x;
} else {
    return 0;
}
```

#### While Statement

```
<while_stmt> ::= "while" <expression> <block> ;
```

The condition is not parenthesized.

#### For Statement

```
<for_stmt>  ::= "for" "(" <for_init> ";" <expression> ";" <for_post> ")" <block> ;
<for_init>  ::= <var_decl> | <expression> ;
<for_post>  ::= <expression> ;
```

The init clause may declare a new variable. The condition and post expressions are mandatory.

Example:
```
for (mut i32 i = 0; i < 10; i = i + 1) {
    ...
}
```

---

### Expressions

Operators are listed in order of **increasing** precedence (lower rows bind tighter).

| Level | Operators                         | Associativity |
|-------|-----------------------------------|---------------|
| 1     | `=` (assignment)                  | Right         |
| 2     | `\|\|`, `or`                      | Left          |
| 3     | `&&`, `and`                       | Left          |
| 4     | `\|`                              | Left          |
| 5     | `^`                               | Left          |
| 6     | `&`                               | Left          |
| 7     | `==`, `!=`                        | Left          |
| 8     | `<`, `>`, `<=`, `>=`              | Left          |
| 9     | `<<`, `>>`                        | Left          |
| 10    | `+`, `-`                          | Left          |
| 11    | `*`, `/`, `%`                     | Left          |
| 12    | `-`, `!`, `~` (unary prefix)      | Right         |
| 13    | `()`, `.`, `as` (postfix)         | Left          |

#### Grammar Productions

```
<expression>     ::= <assignment> ;

<assignment>     ::= <logical_or> [ "=" <assignment> ] ;

<logical_or>     ::= <logical_and>    { ( "||"  | "or"  ) <logical_and> } ;
<logical_and>    ::= <bitwise_or>     { ( "&&"  | "and" ) <bitwise_or>  } ;

<bitwise_or>     ::= <bitwise_xor>   { "|" <bitwise_xor>  } ;
<bitwise_xor>    ::= <bitwise_and>   { "^" <bitwise_and>  } ;
<bitwise_and>    ::= <equality>      { "&" <equality>     } ;

<equality>       ::= <relational>    { ( "==" | "!=" ) <relational>            } ;
<relational>     ::= <shift>         { ( "<"  | ">"  | "<=" | ">=" ) <shift>   } ;
<shift>          ::= <additive>      { ( "<<" | ">>" ) <additive>              } ;
<additive>       ::= <multiplicative>{ ( "+"  | "-"  ) <multiplicative>        } ;
<multiplicative> ::= <unary>         { ( "*"  | "/"  | "%" ) <unary>           } ;

<unary>          ::= ( "-" | "!" | "~" ) <unary> | <postfix> ;

<postfix>        ::= <primary> { <postfix_tail> } ;
<postfix_tail>   ::= "(" [ <arg_list> ] ")"
                   | "." <identifier>
                   | "as" <type>
                   ;
<arg_list>       ::= <expression> { "," <expression> } ;

<primary>        ::= <identifier>
                   | <integer_literal>
                   | <float_literal>
                   | <char_literal>
                   | <string_literal>
                   | <bool_literal>
                   | "(" <expression> ")"
                   | <struct_literal>
                   ;

<struct_literal>      ::= "{" [ <struct_field_list> ] "}" ;
<struct_field_list>   ::= <struct_field> { "," <struct_field> } [ "," ] ;
<struct_field>        ::= <identifier> ":" <expression> ;
```

#### Assignment

Assignment is an expression and is right-associative. The left-hand side must be an assignable location (a variable or struct field).

#### Cast (`as`)

The `as` postfix operator converts an expression to a target type:

```
<expr> as <type>
```

Cast chains are left-associative: `x as i64 as f64`.

#### Function Calls

A call expression applies a postfix `( arg_list )` to any expression that evaluates to a callable:

```
add(a, b)
obj.method(x)
```

#### Member Access

The `.` operator accesses a field of a struct value:

```
point.x
line.start.y
```

#### Struct Literals

A struct literal constructs a value of a struct type. The type is inferred from context:

```
{ x: 1, y: 2 }
{ a: p1, b: p2, c: 0, }
```

Trailing commas are permitted.
