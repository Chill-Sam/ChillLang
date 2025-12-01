# Lexical grammar
<digit>            ::= "0" | "1" | … | "9" ;
<letter>           ::= "A" … "Z" | "a" … "z" | "_" ;
<integer_literal>  ::= <digit> { <digit> } [ <suffix> ] ;
<suffix>           ::= "u8" | "u16" | "u32" | "u64"
                    | "i8" | "i16" | "i32" | "i64" | "f32" | "f64" ;
<identifier>       ::= <letter> { <letter> | <digit> } ;
<type>          ::= <identifier> ;

## single-character tokens: 
"(" ")" "{" "}" "+" "-" "*" "/" "%" "," ";" "=" "<" ">" "&" "|" "^" "~"

## keywords: 
"fun" "return" "mut" "if" "else" "while" "for" "struct" "and" "or" "not" "as"

# Top-level
<program> ::= { <function_def> } ;

# Function definitions
<function_def>  ::= fun <identifier> ( [ <param_list> ] ) <type> <block> ;

<param_list>    ::= <param> { , <param> } ;
<param>         ::= <type> <identifier> ;

# Blocks & statements
<block>         ::= { { <statement> | <block> } } ;

<statement>     ::=
      <const_decl> ;
    | <mut_decl>   ;
    | <return_stmt> ;
    | <expr_stmt>  ;
    | <assign_stmt> ;
    | <if_stmt>    ;
    ;

<const_decl>    ::= <type> <identifier> = <expression> ;
<mut_decl>      ::= mut <type> <identifier> [ = <expression> ] ;
<return_stmt>   ::= return <expression> ;
<expr_stmt>     ::= <expression> ;
<assign_stmt>   ::= <identifier> = <expression> ;
<if_stmt>       ::= if ( <expression> ) <block> [ else <block> ] ;
<while_stmt>    ::= while ( <expression> ) <block> ;
<for_stmt>      ::= for ( <expression>; <expression>; <expression> ) <block> ;

# Expressions
<expression>    ::= <assignment> ;

<assignment>   ::= <identifier> = <logical_or> ;
<logical_or>   ::= <logical_and> { || | or <logical_and> } ;
<logical_and>  ::= <bitwise_or> { && | and <bitwise_or> } ;
<bitwise_or>   ::= <bitwise_xor> { | <bitwise_xor> } ;
<bitwise_xor>  ::= <bitwise_and> { ^ <bitwise_and> } ;
<bitwise_and>  ::= <equality> { & <equality> } ;
<equality>     ::= <relational> { == | != <relational> } ;
<relational>   ::= <shift> { < | > | <= | >= <shift> } ;
<shift>        ::= <add> { << | >> <add> } ;
<add>          ::= <mul> { + | - <mul> } ;
<mul>          ::= <unary> { * | / | % <unary> } ;
<unary>        ::= { - | ~ } <unary> | <postfix> ;
<postfix>      ::= <primary> <postfix_tail>* ;
<postfix_tail> ::= <call> | <member_access> | as <type> ;
<call>         ::= ( { <expression> ("," <expression>)* } ) ;
<member_access> ::= . <identifier> ;
<primary>       ::= <identifier> | <literal> | ( <expression> ) ;
