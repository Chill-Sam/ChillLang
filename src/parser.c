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

/* <expression> ::= <logical_or_expr> */
ASTNode *parse_expression(void) {
    // TODO: Switch to logical or expr
    ASTNode *logical_or_expr = parse_bitwise_or_expr();
    ASTNode *expr =
        ast_make_expression(logical_or_expr->line, logical_or_expr->column);
    ast_add_child(expr, logical_or_expr);
    return expr;
}

/* <logical_or_expr> ::= <logical_and_expr> { || | or <logical_and_expr> } ; */
ASTNode *parse_logical_or_expr(void) {
    ASTNode *lhs = parse_logical_and_expr();
    while (accept(TOKEN_PIPE)) {
        ASTNode *rhs = parse_logical_and_expr();
        lhs = ast_make_or_expr(lhs, rhs, lhs->line, lhs->column);
    }
    return lhs;
}

/* <logical_and_expr> ::= <bitwise_or_expr> { && | and <bitwise_or_expr> } ; */
ASTNode *parse_logical_and_expr(void) {
    ASTNode *lhs = parse_bitwise_or_expr();
    while (accept(TOKEN_AMPERSAND)) {
        ASTNode *rhs = parse_bitwise_or_expr();
        lhs = ast_make_and_expr(lhs, rhs, lhs->line, lhs->column);
    }
    return lhs;
}

/* <bitwise_or_expr> ::= <bitwise_xor_expr> { | <bitwise_xor_expr> } ; */
ASTNode *parse_bitwise_or_expr(void) {
    ASTNode *lhs = parse_bitwise_xor_expr();
    while (accept(TOKEN_PIPE)) {
        ASTNode *rhs = parse_bitwise_xor_expr();
        lhs = ast_make_or_expr(lhs, rhs, lhs->line, lhs->column);
    }
    return lhs;
}

/* <bitwise_xor_expr> ::= <bitwise_and_expr> { ^ <bitwise_and_expr> } ; */
ASTNode *parse_bitwise_xor_expr(void) {
    ASTNode *lhs = parse_bitwise_and_expr();
    while (accept(TOKEN_XOR)) {
        ASTNode *rhs = parse_bitwise_and_expr();
        lhs = ast_make_xor_expr(lhs, rhs, lhs->line, lhs->column);
    }
    return lhs;
}

/* <bitwise_and_expr> ::= <equality_expr> { & <equality_expr> } ; */
ASTNode *parse_bitwise_and_expr(void) {
    ASTNode *lhs = parse_equality_expr();
    while (accept(TOKEN_AMPERSAND)) {
        ASTNode *rhs = parse_equality_expr();
        lhs = ast_make_and_expr(lhs, rhs, lhs->line, lhs->column);
    }
    return lhs;
}

/* <equality_expr> ::= <relational_expr> { == | != <relational_expr> } ; */
ASTNode *parse_equality_expr(void) {
    ASTNode *lhs = parse_relational_expr();
    while (peek_token().type == TOKEN_LOGICAL_EQUALS ||
           peek_token().type == TOKEN_LOGICAL_NOT_EQUALS) {
        if (accept(TOKEN_LOGICAL_EQUALS)) {
            ASTNode *rhs = parse_relational_expr();
            lhs =
                ast_make_logical_equals_expr(lhs, rhs, lhs->line, lhs->column);
        } else if (accept(TOKEN_LOGICAL_NOT_EQUALS)) {
            ASTNode *rhs = parse_relational_expr();
            lhs = ast_make_logical_not_equals_expr(lhs, rhs, lhs->line,
                                                   lhs->column);
        } else {
            write(STDOUT_FILENO, "ERROR PARSING EQUALITY_EXPR\n", 30);
            return NULL;
        }
    }
    return lhs;
}

/* <relational_expr> ::= <shift_expr> { < | > | <= | >= <shift_expr> } ; */
ASTNode *parse_relational_expr(void) {
    ASTNode *lhs = parse_shift_expr();
    while (peek_token().type == TOKEN_LOGICAL_LESS ||
           peek_token().type == TOKEN_LOGICAL_GREATER ||
           peek_token().type == TOKEN_LOGICAL_LESS_EQUALS ||
           peek_token().type == TOKEN_LOGICAL_GREATER_EQUALS) {
        if (accept(TOKEN_LOGICAL_LESS)) {
            ASTNode *rhs = parse_shift_expr();
            lhs = ast_make_logical_less_expr(lhs, rhs, lhs->line, lhs->column);
        } else if (accept(TOKEN_LOGICAL_GREATER)) {
            ASTNode *rhs = parse_shift_expr();
            lhs =
                ast_make_logical_greater_expr(lhs, rhs, lhs->line, lhs->column);
        } else if (accept(TOKEN_LOGICAL_LESS_EQUALS)) {
            ASTNode *rhs = parse_shift_expr();
            lhs = ast_make_logical_less_equals_expr(lhs, rhs, lhs->line,
                                                    lhs->column);
        } else if (accept(TOKEN_LOGICAL_GREATER_EQUALS)) {
            ASTNode *rhs = parse_shift_expr();
            lhs = ast_make_logical_greater_equals_expr(lhs, rhs, lhs->line,
                                                       lhs->column);
        } else {
            write(STDOUT_FILENO, "ERROR PARSING RELATIONAL_EXPR\n", 30);
            return NULL;
        }
    }
    return lhs;
}

/* <shift_expr> ::= <add_expr> { << | >> } <add_expr> */
ASTNode *parse_shift_expr(void) {
    ASTNode *lhs = parse_add_expr();
    while (peek_token().type == TOKEN_SHIFT_LEFT ||
           peek_token().type == TOKEN_SHIFT_RIGHT) {
        if (accept(TOKEN_SHIFT_LEFT)) {
            ASTNode *rhs = parse_add_expr();
            lhs = ast_make_shift_left(lhs, rhs, lhs->line, lhs->column);
        } else if (accept(TOKEN_SHIFT_RIGHT)) {
            ASTNode *rhs = parse_add_expr();
            lhs = ast_make_shift_right(lhs, rhs, lhs->line, lhs->column);
        } else {
            write(STDOUT_FILENO, "ERROR PARSING SHIFT_EXPR\n", 24);
            return NULL;
        }
    }

    return lhs;
}

/* <add_expr> ::= <mul_expr> { ( + | - ) <mul_expr> } ; */
ASTNode *parse_add_expr(void) {
    ASTNode *lhs = parse_mul_expr();
    while (peek_token().type == TOKEN_PLUS ||
           peek_token().type == TOKEN_MINUS) {
        if (accept(TOKEN_PLUS)) {
            ASTNode *rhs = parse_mul_expr();
            lhs = ast_make_add_expr(lhs, rhs, lhs->line, lhs->column);
        } else if (accept(TOKEN_MINUS)) {
            ASTNode *rhs = parse_mul_expr();
            lhs = ast_make_sub_expr(lhs, rhs, lhs->line, lhs->column);
        } else {
            write(STDOUT_FILENO, "ERROR PARSING ADD_EXPR\n", 23);
            return NULL;
        }
    }

    return lhs;
}

/* <mul_expr> ::= <unary_expr> { * | / | % <unary_expr> } ; */
ASTNode *parse_mul_expr(void) {
    ASTNode *lhs = parse_unary_expr();
    while (peek_token().type == TOKEN_STAR ||
           peek_token().type == TOKEN_SLASH ||
           peek_token().type == TOKEN_PERCENT) {
        if (accept(TOKEN_STAR)) {
            ASTNode *rhs = parse_unary_expr();
            lhs = ast_make_mul_expr(lhs, rhs, lhs->line, lhs->column);
        } else if (accept(TOKEN_SLASH)) {
            ASTNode *rhs = parse_unary_expr();
            lhs = ast_make_div_expr(lhs, rhs, lhs->line, lhs->column);
        } else if (accept(TOKEN_PERCENT)) {
            ASTNode *rhs = parse_unary_expr();
            lhs = ast_make_mod_expr(lhs, rhs, lhs->line, lhs->column);
        } else {
            write(STDOUT_FILENO, "ERROR PARSING MUL_EXPR\n", 23);
            return NULL;
        }
    }

    return lhs;
}

/* <unary_expr>    ::= { - | ~ } <primary> */
ASTNode *parse_unary_expr(void) {
    ASTNode *expr = NULL;
    if (peek_token().type == TOKEN_MINUS &&
        peek_nth(2).type >= TOKEN_I8_LITERAL &&
        peek_nth(2).type <= TOKEN_U64_LITERAL) {
        expr = parse_primary();
    } else if (accept(TOKEN_MINUS)) {
        ASTNode *rhs = parse_primary();
        expr = ast_make_neg_expr(rhs, rhs->line, rhs->column);
    } else if (accept(TOKEN_NOT)) {
        ASTNode *rhs = parse_primary();
        expr = ast_make_not_expr(rhs, rhs->line, rhs->column);
    } else {
        expr = parse_primary();
    }

    return expr;
}

/* <primary> ::= int_lit | "-" int_lit | ident | call_expr | "(" expr
 * ")" */
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
            parse_errors++;
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
    parse_errors++;
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
