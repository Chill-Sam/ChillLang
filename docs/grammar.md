# Lexical grammar
<digit>            ::= "0" | "1" | … | "9" ;
<letter>           ::= "A" … "Z" | "a" … "z" | "_" ;
<integer_literal>  ::= <digit> { <digit> } [ <suffix> ] ;
<suffix>           ::= "u8" | "u16" | "u32" | "u64"
                    | "i8" | "i16" | "i32" | "i64" ;
<identifier>       ::= <letter> { <letter> | <digit> } ;

## single-character tokens: 
"(" ")" "{" "}" "+" "-" "*" "/" "%" "," ";" "=" "<" ">" "&" "|" "^" "~"

## keywords: 
"return" "mut"

# Top-level
<program> ::= { <function_def> } ;

# Function definitions
<function_def>  ::= <type> <identifier> **(** [ <param_list> ] **)** <block> ;

<param_list>    ::= <param> { **,** <param> } ;
<param>         ::= <type> <identifier> ;

# Blocks & statements
<block>         ::= **{** { <statement> | <block> } **}** ;

<statement>     ::=
      <const_decl> **;**
    | <mut_decl>   **;**
    | <return_stmt> **;**
    | <expr_stmt>  **;**
    | <assign_stmt> **;**
    ;

<const_decl>    ::= <type> <identifier> **=** <expression> ;
<mut_decl>      ::= **mut** <type> <identifier> [ **=** <expression> ] ;
<return_stmt>   ::= **return** <expression> ;
<expr_stmt>     ::= <expression> ;
<assign_stmt>   ::= <identifier> **=** <expression> ;

# Expressions
<expression>    ::= <logical_or_expr> ;

<logical_or_expr> ::= <logical_and_expr> { || | or <logical_and_expr> } ;
<logical_and_expr> ::= <bitwise_or_expr> { && | and <bitwise_or_expr> } ;
<bitwise_or_expr>  ::= <bitwise_xor_expr> { | <bitwise_xor_expr> } ;
<bitwise_xor_expr> ::= <bitwise_and_expr> { ^ <bitwise_and_expr> } ;
<bitwise_and_expr> ::= <equality_expr> { & <equality_expr> } ;
<equality_expr>   ::= <relational_expr> { == | != <relational_expr> } ;
<relational_expr> ::= <shift_expr> { < | > | <= | >= <shift_expr> } ;
<shift_expr>      ::= <add_expr> { << | >> <add_expr> } ;
<add_expr>   ::= <mul_expr> { + | - <mul_expr> } ;
<mul_expr> ::= <unary_expr> { * | / | % <unary_expr> } ;
<unary_expr>    ::= { - | ~ } <primary> ;

<primary>       ::=
      <integer_literal>
    | "-" <integer_literal>
    | <identifier>
    | <call_expr>
    | **(** <expression> **)**
    ;

<call_expr>     ::= <identifier> **(** [ <arg_list> ] **)** ;
<arg_list>      ::= <expression> { **,** <expression> } ;

# Types
<type>          ::= <identifier> ;
