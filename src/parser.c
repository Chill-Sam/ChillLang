#include "parser.h"

#include "AST/ast.c"
#include "AST/ast_arena.c"
#include "AST/ast_builder.c"
#include "AST/ast_log.c"
#include "file_stream.c"
#include "lexer.c"
#include "symtab.c"
#include "tokens.c"

/* <program> ::= { <function_def> } */
ASTNode *parse_program(void) {
    ast_arena_reset();
    symtab_init();
    ASTNode *root = ast_make_program();
    while (peek_token().type != TOKEN_EOF) {
        ASTNode *fn = parse_function_def();
        ast_add_child(root, fn);
    }
    return root;
}

/* <function_def> ::= <type> <identifier> ( <param_list> ) <block> */
ASTNode *parse_function_def(void) {
    Token tk_ret = expect(TOKEN_IDENTIFIER);
    ASTNode *ret_ty = ast_make_type_name(tk_ret);

    Token tk_name = expect(TOKEN_IDENTIFIER);
    ASTNode *name = ast_make_identifier(tk_name);

    expect(TOKEN_LPAREN);  // Starting left parentheses
    ASTNode *params = ast_make_param_list(tk_name.line, tk_name.column);
    if (!accept(TOKEN_RPAREN)) {  // If no right parantheses immedietly after,
                                  // must be a param list
        do {
            // each <param> ::= <type> <ident>
            Token p_ty_tok = expect(TOKEN_IDENTIFIER);
            ASTNode *p_ty = ast_make_type_name(p_ty_tok);

            Token p_nm_tok = expect(TOKEN_IDENTIFIER);
            ASTNode *p_nm = ast_make_identifier(p_nm_tok);

            ast_add_child(params, ast_make_param(p_ty, p_nm));
        } while (
            accept(TOKEN_COMMA));  // While there are params seperated by commas
        expect(TOKEN_RPAREN);  // Ending parentheses
    }

    ASTNode *body = parse_block();  // Following body of the function
    ASTNode *func = ast_make_function_def(ret_ty, name, params, body);
    symtab_add_function(func);
    return func;
}

/* <block> ::= "{" { <statement> | <block> } "}" */
ASTNode *parse_block(void) {
    Token tk = expect(TOKEN_LBRACE);
    ASTNode *block = ast_make_block(tk.line, tk.column);

    // Keep parsing statements until right brace
    while (!accept(TOKEN_RBRACE)) {
        if (peek_token().type == TOKEN_LBRACE) {
            ast_add_child(block, parse_block());
        } else {
            ast_add_child(block, parse_statement());
        }
    }

    return block;
}

/* <statement> ::= const_decl ";" | mut_decl ";" | return_stmt ";" | expr_stmt
 * ";" | <assign_stmt> ";" */
ASTNode *parse_statement(void) {
    // <const_decl> ::= <type> <identifier> = <expression>
    // We check until equals sign to differentiate from <expression>
    if (peek_nth(1).type == TOKEN_IDENTIFIER &&
        peek_nth(2).type == TOKEN_IDENTIFIER &&
        peek_nth(3).type == TOKEN_EQUALS) {
        ASTNode *const_decl = parse_const_decl();
        expect(TOKEN_SEMI);
        return const_decl;
    }

    if (peek_nth(1).type == TOKEN_IDENTIFIER &&
        peek_nth(2).type == TOKEN_EQUALS) {
        ASTNode *assign_stmt = parse_assign_stmt();
        expect(TOKEN_SEMI);
        return assign_stmt;
    }

    // <mut_decl> ::= mut <type> <identifier> = <expression>
    if (peek_token().type == TOKEN_MUT) {
        ASTNode *mut_decl = parse_mut_decl();
        expect(TOKEN_SEMI);
        return mut_decl;
    }

    // <return_stmt> ::= return <expression>
    if (peek_token().type == TOKEN_RETURN) {
        ASTNode *return_stmt = parse_return_stmt();
        expect(TOKEN_SEMI);
        return return_stmt;
    }

    // Else parse expression
    ASTNode *expr_stmt = parse_expr_stmt();
    expect(TOKEN_SEMI);
    return expr_stmt;
}

/* <const_decl> ::= <type> <ident> "=" <expression> */
ASTNode *parse_const_decl(void) {
    Token tk_type = expect(TOKEN_IDENTIFIER);
    ASTNode *type = ast_make_type_name(tk_type);

    Token tk_name = expect(TOKEN_IDENTIFIER);
    ASTNode *name = ast_make_identifier(tk_name);

    expect(TOKEN_EQUALS);

    ASTNode *init = parse_expression();

    ASTNode *const_decl =
        ast_make_const_decl(type, name, init, type->line, type->column);
    return const_decl;
}

/* <mut_decl> ::= mut <type> <identifier> = <expression> */
ASTNode *parse_mut_decl(void) {
    expect(TOKEN_MUT);

    Token tk_type = expect(TOKEN_IDENTIFIER);
    ASTNode *type = ast_make_type_name(tk_type);

    Token tk_name = expect(TOKEN_IDENTIFIER);
    ASTNode *name = ast_make_identifier(tk_name);

    ASTNode *init = NULL;
    if (accept(TOKEN_EQUALS)) {
        init = parse_expression();
    }
    ASTNode *mut_decl =
        ast_make_mut_decl(type, name, init, type->line, type->column);
    return mut_decl;
}

/* <return_stmt> ::= return <expression> */
ASTNode *parse_return_stmt(void) {
    Token tk_ret = expect(TOKEN_RETURN);  // Used for line and column
    ASTNode *expr = parse_expression();
    return ast_make_return_stmt(expr, tk_ret.line, tk_ret.column);
}

/* <expr_stmt> ::= <expression> */
ASTNode *parse_expr_stmt(void) {
    ASTNode *expr = parse_expression();
    return ast_make_expr_stmt(expr, expr->line, expr->column);
}

/* <assign_stmt> ::= <identifier> = <expression> */
ASTNode *parse_assign_stmt(void) {
    Token id = next_token();
    expect(TOKEN_EQUALS);
    ASTNode *rhs = parse_expression();

    ASTNode *assign_stmt =
        ast_make_assign(ast_make_identifier(id), rhs, id.line, id.column);
    return assign_stmt;
}

/* <expression> ::= <add_expr> */
ASTNode *parse_expression(void) {
    ASTNode *add_expr = parse_add_expr();
    ASTNode *expr = ast_make_expression(add_expr->line, add_expr->column);
    ast_add_child(expr, add_expr);
    return expr;
}

/* <add_expr> ::= <primary> { "+" <primary> } */
ASTNode *parse_add_expr(void) {
    ASTNode *lhs = parse_primary();
    while (accept(TOKEN_PLUS)) {
        ASTNode *rhs = parse_primary();
        lhs = ast_make_add_expr(lhs, rhs, lhs->line, lhs->column);
    }

    return lhs;
}

/* <primary> ::= int_lit | "-" int_lit | ident | call_expr | "(" expr ")" */
ASTNode *parse_primary(void) {
    Token tk = peek_token();

    if (tk.type >= TOKEN_I8_LITERAL && tk.type <= TOKEN_U64_LITERAL) {
        next_token();
        return ast_make_int_literal(tk);
    }

    if (tk.type == TOKEN_MINUS) {
        if (peek_nth(2).type >= TOKEN_I8_LITERAL &&
            peek_nth(2).type <= TOKEN_U64_LITERAL) {
            next_token();
            Token next = next_token();
            next.data.literal.i *= -1;
            return ast_make_int_literal(next);
        } else {
            write(STDOUT_FILENO, "Error parsing primary\n", 22);
            output_token(peek_nth(2));
            return NULL;
        }
    }

    if (tk.type == TOKEN_IDENTIFIER && peek_nth(2).type == TOKEN_LPAREN) {
        return parse_call_expr();
    }

    // Not a call_expr but still an identifier
    if (tk.type == TOKEN_IDENTIFIER) {
        next_token();
        return ast_make_identifier(tk);
    }

    if (tk.type == TOKEN_LPAREN) {
        next_token();
        ASTNode *expr = parse_expression();
        expect(TOKEN_RPAREN);
        return expr;
    }

    write(STDOUT_FILENO, "Error parsing primary\n", 22);
    output_token(tk);
    return NULL;
}

/* <call_expr> ::= <identifier> ( <arg_list> ) */
ASTNode *parse_call_expr(void) {
    Token id = expect(TOKEN_IDENTIFIER);
    expect(TOKEN_LPAREN);

    ASTNode *arg_list = NULL;
    if (!accept(TOKEN_RPAREN)) {
        arg_list = parse_arg_list();
        expect(TOKEN_RPAREN);
    }

    ASTNode *call_expr = ast_make_call_expr(ast_make_identifier(id), arg_list,
                                            id.line, id.column);
    return call_expr;
}

/* <arg_list> ::= <expression> { , <expression> } */
ASTNode *parse_arg_list(void) {
    Token tk = peek_token();
    ASTNode *arg_list = ast_make_arg_list(tk.line, tk.column);
    do {
        ASTNode *arg = parse_expression();
        ast_add_child(arg_list, arg);
    } while (accept(TOKEN_COMMA));

    return arg_list;
}
