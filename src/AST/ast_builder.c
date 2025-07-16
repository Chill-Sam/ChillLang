// -----------------------------------------------------------------------------
// Builder functions for each grammar production
// -----------------------------------------------------------------------------

#pragma once

#include "../tokens.c"
#include "ast.c"
#include "ast_arena.c"
#include "ast_log.c"

// Forward decl
ASTNode *ast_make_type_name(Token tok);

// <program> ::= { <function_def> } ;
ASTNode *ast_make_program(void) {
    // line/column don't really matter for the root
    return ast_new(AST_PROGRAM, 0, 0);
}

// <function_def> ::= <type> <identifier> "(" [<param_list>] ")" <block>
ASTNode *ast_make_function_def(ASTNode *ret_type, ASTNode *name,
                               ASTNode *param_list,  // may be NULL if no params
                               ASTNode *body)        // an AST_BLOCK
{
    // Use the function's name location for the node
    ASTNode *fn = ast_new(AST_FUNCTION_DEF, name->line, name->column);
    ast_add_child(fn, ret_type);
    ast_add_child(fn, name);
    ast_add_child(fn, param_list);
    ast_add_child(fn, body);
    return fn;
}

// <param_list> ::= <param> { "," <param> }
ASTNode *ast_make_param_list(int line, int column) {
    return ast_new(AST_PARAM_LIST, line, column);
}

// <param> ::= <type> <identifier>
ASTNode *ast_make_param(ASTNode *type, ASTNode *name) {
    ASTNode *p = ast_new(AST_PARAM, name->line, name->column);
    ast_add_child(p, type);
    ast_add_child(p, name);
    return p;
}

// <block> ::= "{" { <statement> } "}"
ASTNode *ast_make_block(int line, int column) {
    return ast_new(AST_BLOCK, line, column);
}

// <return_stmt> ::= "return" <expression>
ASTNode *ast_make_return_stmt(ASTNode *expr, int line, int column) {
    ASTNode *r = ast_new(AST_RETURN_STMT, line, column);
    ast_add_child(r, expr);
    return r;
}

// <const_decl> ::= <type> <identifier> "=" <expression>
ASTNode *ast_make_const_decl(ASTNode *type, ASTNode *name, ASTNode *init,
                             int line, int column) {
    ASTNode *d = ast_new(AST_CONST_DECL, line, column);
    ast_add_child(d, type);
    ast_add_child(d, name);
    ast_add_child(d, init);
    return d;
}

// <mut_decl> ::= "mut" <type> <identifier> [ "=" <expression> ]
ASTNode *ast_make_mut_decl(ASTNode *type, ASTNode *name,
                           ASTNode *init,  // may be NULL
                           int line, int column) {
    ASTNode *d = ast_new(AST_MUT_DECL, line, column);
    ast_add_child(d, type);
    ast_add_child(d, name);
    if (init) ast_add_child(d, init);
    return d;
}

// <expr_stmt> ::= <expression>
ASTNode *ast_make_expr_stmt(ASTNode *expr, int line, int column) {
    ASTNode *s = ast_new(AST_EXPR_STMT, line, column);
    ast_add_child(s, expr);
    return s;
}

/* <assign_stmt> ::= <identifier> = <expression> */
ASTNode *ast_make_assign(ASTNode *lhs, ASTNode *rhs, int line, int col) {
    ASTNode *n = ast_new(AST_ASSIGN_STMT, line, col);

    ast_add_child(n, lhs);
    ast_add_child(n, rhs);
    return n;
}

/* <expression> ::= <add_expr> */
ASTNode *ast_make_expression(int line, int column) {
    ASTNode *s = ast_new(AST_EXPRESSION, line, column);
    return s;
}

// <add_expr> ::= <primary> { "+" <primary> }
ASTNode *ast_make_add_expr(ASTNode *lhs, ASTNode *rhs, int line, int column) {
    ASTNode *e = ast_new(AST_ADD_EXPR, line, column);
    ast_add_child(e, lhs);
    ast_add_child(e, rhs);
    return e;
}

// <add_expr> ::= <primary> { "+" <primary> }
ASTNode *ast_make_sub_expr(ASTNode *lhs, ASTNode *rhs, int line, int column) {
    ASTNode *e = ast_new(AST_SUB_EXPR, line, column);
    ast_add_child(e, lhs);
    ast_add_child(e, rhs);
    return e;
}

ASTNode *ast_make_mul_expr(ASTNode *lhs, ASTNode *rhs, int line, int column) {
    ASTNode *e = ast_new(AST_MUL_EXPR, line, column);
    ast_add_child(e, lhs);
    ast_add_child(e, rhs);
    return e;
}

ASTNode *ast_make_div_expr(ASTNode *lhs, ASTNode *rhs, int line, int column) {
    ASTNode *e = ast_new(AST_DIV_EXPR, line, column);
    ast_add_child(e, lhs);
    ast_add_child(e, rhs);
    return e;
}

ASTNode *ast_make_mod_expr(ASTNode *lhs, ASTNode *rhs, int line, int column) {
    ASTNode *e = ast_new(AST_MOD_EXPR, line, column);
    ast_add_child(e, lhs);
    ast_add_child(e, rhs);
    return e;
}

// <call_expr> ::= <identifier> "(" [ <arg_list> ] ")"
ASTNode *ast_make_call_expr(ASTNode *fn,
                            ASTNode *arg_list,  // may be NULL
                            int line, int column) {
    ASTNode *c = ast_new(AST_CALL_EXPR, line, column);
    ast_add_child(c, fn);
    ast_add_child(c, arg_list);
    return c;
}

// <arg_list> ::= <expression> { "," <expression> }
ASTNode *ast_make_arg_list(int line, int column) {
    return ast_new(AST_ARG_LIST, line, column);
}

// <integer_literal>
ASTNode *ast_make_int_literal(Token tok) {
    ASTNode *n = ast_new(AST_INT_LITERAL, tok.line, tok.column);

    if (tok.type >= TOKEN_I8_LITERAL && tok.type <= TOKEN_INT_LITERAL) {
        n->data.int_lit.i_val = tok.data.literal.i;
        n->data.int_lit.u_val = 0;
    } else {
        n->data.int_lit.u_val = tok.data.literal.u;
        n->data.int_lit.i_val = 0;
    }
    const char *type_name;
    switch (tok.type) {
        case TOKEN_I8_LITERAL:
            type_name = "i8";
            break;
        case TOKEN_I16_LITERAL:
            type_name = "i16";
            break;
        case TOKEN_I32_LITERAL:
            type_name = "i32";
            break;
        case TOKEN_I64_LITERAL:
            type_name = "i64";
            break;
        case TOKEN_U8_LITERAL:
            type_name = "u8";
            break;
        case TOKEN_U16_LITERAL:
            type_name = "u16";
            break;
        case TOKEN_U32_LITERAL:
            type_name = "u32";
            break;
        case TOKEN_U64_LITERAL:
            type_name = "u64";
            break;
        case TOKEN_INT_LITERAL:
            return n;
        default:
            type_name = "i32";
    }

    Token ty_tok;
    ty_tok.line = tok.line;

    ty_tok.column = tok.column;
    ty_tok.type = TOKEN_IDENTIFIER;
    ty_tok.length = str_len(type_name);
    if (ty_tok.length > MAX_LEXEME) ty_tok.length = MAX_LEXEME;

    for (int i = 0; i < ty_tok.length; i++) {
        ty_tok.lexeme[i] = type_name[i];
    }
    ty_tok.lexeme[ty_tok.length] = '\0';
    n->data.int_lit.type = ast_make_type_name(ty_tok);

    return n;
}

// <identifier>
ASTNode *ast_make_identifier(Token tok) {
    ASTNode *n = ast_new(AST_IDENTIFIER, tok.line, tok.column);
    n->data.text.name = ast_strdup(tok.lexeme, tok.length);
    return n;
}

// <type>  (same form as identifier, but different kind)
ASTNode *ast_make_type_name(Token tok) {
    ASTNode *n = ast_new(AST_TYPE_NAME, tok.line, tok.column);
    n->data.text.name = ast_strdup(tok.lexeme, tok.length);
    return n;
}
