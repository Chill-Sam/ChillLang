#include "parser.h"
#include <stdlib.h>

static void advance(Parser *p) {
    if (p->has_peek) {
        p->cur      = p->peek;
        p->has_peek = 0;
    } else {
        p->cur = lexer_next(p->lx);
    }
}

static Token peek(Parser *p) {
    if (!p->has_peek) {
        p->peek     = lexer_next(p->lx);
        p->has_peek = 1;
    }
    return p->peek;
}

static int match(Parser *p, TokenKind kind) {
    if (p->cur.kind == kind) {
        advance(p);
        return 1;
    }
    return 0;
}

static Token expect(Parser *p, TokenKind kind, const char *msg) {
    if (p->cur.kind != kind) {
        // TODO: Hook into diagnostic system
        fprintf(stderr, "parse error at %u:%u: expected %s, got %s : %s\n",
                p->cur.line, p->cur.column, token_kind_name(kind),
                token_kind_name(p->cur.kind), msg);
        exit(1);
    }
    Token t = p->cur;
    advance(p);
    return t;
}

void parser_init(Parser *p, Lexer *lx) {
    p->lx       = lx;
    p->has_peek = 0;
    p->cur      = lexer_next(lx);
}

AstNode *new_node(AstNodeKind kind) {
    AstNode *n = malloc(sizeof *n);
    n->kind    = kind;
    return n;
}

static AstNode *make_ident_node(Token name) {
    AstNode *n            = new_node(AST_IDENT_EXPR);
    n->as.ident_expr.name = name;
    return n;
}

static AstNode *make_literal_node(Token tok) {
    AstNode *n             = new_node(AST_LITERAL_EXPR);
    n->as.literal_expr.tok = tok;
    return n;
}

static AstNode *parse_type_spec(Parser *p) {
    Token name = expect(p, TOK_IDENT, "expected type name");
    return make_ident_node(name);
}

// Forward declarations
static AstNode *parse_block(Parser *p);
static AstNode *parse_func_decl(Parser *p);
static AstNode *parse_struct_decl(Parser *p);
static AstNode *parse_stmt(Parser *p);
static AstNode *parse_expr(Parser *p);

AstNode *parse_translation_unit(Parser *p) {
    AstNode *root = new_node(AST_TRANSLATION_UNIT);
    ast_list_init(&root->as.translation_unit.items);

    while (p->cur.kind != TOK_EOF) {
        AstNode *item = NULL;

        // TODO: Allow top-level varaible declarations
        if (p->cur.kind == TOK_KW_FUN) {
            item = parse_func_decl(p);
        } else if (p->cur.kind == TOK_KW_STRUCT) {
            item = parse_struct_decl(p);
        } else {
            // TODO: Hook into diagnostic system
            fprintf(
                stderr,
                "parse error at %u:%u: unexpected token at top level: '%.*s'\n",
                p->cur.line, p->cur.column, (int)p->cur.length, p->cur.lexeme);
            abort();
        }
        ast_list_push(&root->as.translation_unit.items, item);
    }

    return root;
}

void free_translation_unit(AstNode *tu) {
    ast_list_free(&tu->as.translation_unit.items);
    free(tu);
}

// ----- Function parsing -----
static AstNode *parse_param(Parser *p) {
    bool is_mut = false;
    if (p->cur.kind == TOK_KW_MUT) {
        is_mut = true;
        advance(p);
    }

    AstNode *ty = parse_type_spec(p);
    Token name  = expect(p, TOK_IDENT, "expected parameter name");

    // Create new param node
    AstNode *param         = new_node(AST_PARAM);
    param->as.param.type   = ty;
    param->as.param.is_mut = is_mut;
    param->as.param.name   = name;
    return param;
}

static AstNode *parse_params(Parser *p, AstNode *fn) {
    expect(p, TOK_LPAREN, "expected '(' after function name");
    ast_list_init(&fn->as.func_decl.params);

    if (p->cur.kind != TOK_RPAREN) {
        for (;;) {
            AstNode *param = parse_param(p);
            ast_list_push(&fn->as.func_decl.params, param);
            if (!match(p, TOK_COMMA))
                break;
        }
    }

    expect(p, TOK_RPAREN, "expected ')' after parameters");
    return fn;
}

static AstNode *parse_func_decl(Parser *p) {
    expect(p, TOK_KW_FUN, "expected 'fun' keyword");

    Token name = expect(p, TOK_IDENT, "expected function name");

    // Create new function node
    AstNode *fn                  = new_node(AST_FUNC_DECL);
    fn->as.func_decl.name        = name;
    fn->as.func_decl.return_type = NULL;
    fn->as.func_decl.body        = NULL;
    ast_list_init(&fn->as.func_decl.params);

    parse_params(p, fn);

    fn->as.func_decl.return_type =
        p->cur.kind == TOK_LBRACE
            ? make_ident_node((Token){.kind   = TOK_IDENT,
                                      .offset = p->cur.offset,
                                      .length = 4,
                                      .line   = p->cur.line,
                                      .column = p->cur.column,
                                      "void"})
            : parse_type_spec(p);
    fn->as.func_decl.body = parse_block(p);

    return fn;
}

// ----- Struct parsing -----
static AstNode *parse_field(Parser *p) {
    Token name = expect(p, TOK_IDENT, "expected field name");
    expect(p, TOK_COLON, "expected ':' after field name");

    AstNode *ty          = parse_type_spec(p);
    AstNode *field       = new_node(AST_FIELD);
    field->as.field.name = name;
    field->as.field.type = ty;
    return field;
}

static AstNode *parse_struct_decl(Parser *p) {
    expect(p, TOK_KW_STRUCT, "expected 'struct' keyword");
    Token name = expect(p, TOK_IDENT, "expected struct name");
    expect(p, TOK_LBRACE, "expected '{' after struct name");

    AstNode *struct_decl             = new_node(AST_STRUCT_DECL);
    struct_decl->as.struct_decl.name = name;
    ast_list_init(&struct_decl->as.struct_decl.fields);

    if (p->cur.kind != TOK_RBRACE) {
        for (;;) {
            AstNode *field = parse_field(p);
            ast_list_push(&struct_decl->as.struct_decl.fields, field);

            if (!match(p, TOK_COMMA))
                break;

            // Allow trailing comma
            if (p->cur.kind == TOK_RBRACE)
                break;
        }
    }

    expect(p, TOK_RBRACE, "expected '}' after struct body");
    return struct_decl;
}

// ----- Block + Statement parsing -----
static AstNode *parse_block(Parser *p) {
    expect(p, TOK_LBRACE, "expected '{'");

    AstNode *block = new_node(AST_BLOCK_STMT);
    ast_list_init(&block->as.block_stmt.stmts);

    while (p->cur.kind != TOK_RBRACE && p->cur.kind != TOK_EOF) {
        AstNode *stmt = parse_stmt(p);
        ast_list_push(&block->as.block_stmt.stmts, stmt);
    }

    expect(p, TOK_RBRACE, "expected '}'");
    return block;
}

static int starts_var_decl(Parser *p) {
    // we check for MUT or IDENT IDENT
    if (p->cur.kind == TOK_KW_MUT)
        return 1;

    if (p->cur.kind != TOK_IDENT)
        return 0;

    Token next = peek(p);

    if (next.kind == TOK_IDENT) {
        return 1;
    }

    return 0;
}

static AstNode *parse_if_stmt(Parser *p) {
    expect(p, TOK_KW_IF, "expected 'if' keyword");
    AstNode *cond       = parse_expr(p);
    AstNode *then_block = parse_block(p);

    AstNode *else_block = NULL;
    if (match(p, TOK_KW_ELSE)) {
        if (p->cur.kind == TOK_KW_IF) {
            else_block = parse_if_stmt(p);
        } else {
            else_block = parse_block(p);
        }
    }

    AstNode *if_stmt               = new_node(AST_IF_STMT);
    if_stmt->as.if_stmt.cond       = cond;
    if_stmt->as.if_stmt.then_block = then_block;
    if_stmt->as.if_stmt.else_block = else_block;
    return if_stmt;
}

static AstNode *parse_stmt(Parser *p) {
    if (p->cur.kind == TOK_KW_RETURN) {
        advance(p);
        AstNode *ret = new_node(AST_RETURN_STMT);
        if (p->cur.kind != TOK_SEMI) {
            ret->as.return_stmt.expr = parse_expr(p);
        } else {
            ret->as.return_stmt.expr = NULL;
        }
        expect(p, TOK_SEMI, "expected ';' after return statement");
        return ret;
    }

    if (p->cur.kind == TOK_KW_IF) {
        return parse_if_stmt(p);
    }

    if (p->cur.kind == TOK_LBRACE) {
        return parse_block(p);
    }

    if (starts_var_decl(p)) {
        bool is_mut = false;
        if (p->cur.kind == TOK_KW_MUT) {
            is_mut = true;
            advance(p);
        }

        AstNode *type = parse_type_spec(p);
        Token name    = expect(p, TOK_IDENT, "expected variable name");

        AstNode *init = NULL;
        if (p->cur.kind == TOK_EQ) {
            advance(p);
            init = parse_expr(p);
        }

        expect(p, TOK_SEMI, "expected ';' after variable declaration");

        AstNode *var_decl            = new_node(AST_VAR_DECL);
        var_decl->as.var_decl.name   = name;
        var_decl->as.var_decl.type   = type;
        var_decl->as.var_decl.is_mut = is_mut;
        var_decl->as.var_decl.init   = init;
        return var_decl;
    }

    // TODO: Add while, for, etc.

    AstNode *expr = parse_expr(p);
    expect(p, TOK_SEMI, "expected ';' after expression");
    AstNode *stmt           = new_node(AST_EXPR_STMT);
    stmt->as.expr_stmt.expr = expr;
    return stmt;
}

// ----- Expression parsing -----
// ----- Unary expressions -----
static AstNode *parse_primary(Parser *p) {
    if (p->cur.kind == TOK_IDENT) {
        Token name = p->cur;
        advance(p);
        return make_ident_node(name);
    }

    if (p->cur.kind == TOK_INT_LITERAL || p->cur.kind == TOK_FLOAT_LITERAL ||
        p->cur.kind == TOK_STRING_LITERAL || p->cur.kind == TOK_CHAR_LITERAL ||
        p->cur.kind == TOK_KW_TRUE || p->cur.kind == TOK_KW_FALSE) {
        Token lit = p->cur;
        advance(p);
        return make_literal_node(lit);
    }

    if (match(p, TOK_LPAREN)) {
        AstNode *inner = parse_expr(p);
        expect(p, TOK_RPAREN, "expected ')' after expression");
        return inner;
    }

    // TODO: Hook into diagnostic system
    fprintf(stderr,
            "parse error at %u:%u: unexpected token in primary expression: "
            "'%.*s'\n",
            p->cur.line, p->cur.column, (int)p->cur.length, p->cur.lexeme);
    abort();
}

static AstNode *parse_postfix(Parser *p) {
    AstNode *expr = parse_primary(p);

    for (;;) {
        if (p->cur.kind == TOK_LPAREN) {
            // Function call
            advance(p);

            AstNode *call             = new_node(AST_CALL_EXPR);
            call->as.call_expr.callee = expr;
            ast_list_init(&call->as.call_expr.args);

            if (p->cur.kind != TOK_RPAREN) {
                for (;;) {
                    AstNode *arg = parse_expr(p);
                    ast_list_push(&call->as.call_expr.args, arg);
                    if (!match(p, TOK_COMMA))
                        break;
                }
            }

            expect(p, TOK_RPAREN, "expected ')' after function call");
            expr = call;
            continue;
        }

        if (p->cur.kind == TOK_DOT) {
            // Member access
            advance(p);
            Token field     = expect(p, TOK_IDENT, "expected member name");

            AstNode *member = new_node(AST_MEMBER_EXPR);
            member->as.member_expr.base  = expr;
            member->as.member_expr.field = field;
            expr                         = member;
            continue;
        }

        if (p->cur.kind == TOK_KW_AS) {
            // Cast expression
            advance(p);

            AstNode *target           = parse_type_spec(p);
            AstNode *cast             = new_node(AST_CAST_EXPR);
            cast->as.cast_expr.expr   = expr;
            cast->as.cast_expr.target = target;
            expr                      = cast;
            continue;
        }

        return expr;
    }
}

static AstNode *parse_unary(Parser *p) {
    if (p->cur.kind == TOK_MINUS || p->cur.kind == TOK_BANG ||
        p->cur.kind == TOK_TILDE) {
        Token op = p->cur;
        advance(p);

        AstNode *operand = parse_unary(p);

        AstNode *unary   = new_node(AST_UNARY_EXPR);
        switch (op.kind) {
        case TOK_MINUS:
            unary->as.unary_expr.op = UN_NEG;
            break;
        case TOK_BANG:
        case TOK_KW_NOT:
            unary->as.unary_expr.op = UN_NOT;
            break;
        case TOK_TILDE:
            unary->as.unary_expr.op = UN_BITNOT;
            break;
        default:
            fprintf(stderr,
                    "parse error at %u:%u: unreachable in parse_unary\n",
                    p->cur.line, p->cur.column);
            abort();
        }

        unary->as.unary_expr.expr = operand;
        return unary;
    }

    return parse_postfix(p);
}

// ----- Binary expressions -----
static AstNode *make_bin(AstBinOp op, AstNode *lhs, AstNode *rhs) {
    AstNode *bin         = new_node(AST_BIN_EXPR);
    bin->as.bin_expr.op  = op;
    bin->as.bin_expr.lhs = lhs;
    bin->as.bin_expr.rhs = rhs;
    return bin;
}

static AstNode *parse_multiplicative(Parser *p) {
    AstNode *expr = parse_unary(p);
    for (;;) {
        if (p->cur.kind == TOK_STAR) {
            advance(p);
            AstNode *rhs = parse_unary(p);
            expr         = make_bin(BIN_MUL, expr, rhs);
        } else if (p->cur.kind == TOK_SLASH) {
            advance(p);
            AstNode *rhs = parse_unary(p);
            expr         = make_bin(BIN_DIV, expr, rhs);
        } else if (p->cur.kind == TOK_PERCENT) {
            advance(p);
            AstNode *rhs = parse_unary(p);
            expr         = make_bin(BIN_MOD, expr, rhs);
        } else {
            break;
        }
    }

    return expr;
}

static AstNode *parse_additive(Parser *p) {
    AstNode *expr = parse_multiplicative(p);

    for (;;) {
        if (p->cur.kind == TOK_PLUS) {
            advance(p);
            AstNode *rhs = parse_multiplicative(p);
            expr         = make_bin(BIN_ADD, expr, rhs);
        } else if (p->cur.kind == TOK_MINUS) {
            advance(p);
            AstNode *rhs = parse_multiplicative(p);
            expr         = make_bin(BIN_SUB, expr, rhs);
        } else {
            break;
        }
    }

    return expr;
}

static AstNode *parse_shift(Parser *p) {
    AstNode *expr = parse_additive(p);

    for (;;) {
        if (p->cur.kind == TOK_SHL) {
            advance(p);
            AstNode *rhs = parse_additive(p);
            expr         = make_bin(BIN_SHL, expr, rhs);
        } else if (p->cur.kind == TOK_SHR) {
            advance(p);
            AstNode *rhs = parse_additive(p);
            expr         = make_bin(BIN_SHR, expr, rhs);
        } else {
            break;
        }
    }

    return expr;
}

static AstNode *parse_relational(Parser *p) {
    AstNode *expr = parse_shift(p);

    for (;;) {
        if (p->cur.kind == TOK_LT) {
            advance(p);
            AstNode *rhs = parse_shift(p);
            expr         = make_bin(BIN_LT, expr, rhs);
        } else if (p->cur.kind == TOK_GT) {
            advance(p);
            AstNode *rhs = parse_shift(p);
            expr         = make_bin(BIN_GT, expr, rhs);
        } else if (p->cur.kind == TOK_LT_EQ) {
            advance(p);
            AstNode *rhs = parse_shift(p);
            expr         = make_bin(BIN_LE, expr, rhs);
        } else if (p->cur.kind == TOK_GT_EQ) {
            advance(p);
            AstNode *rhs = parse_shift(p);
            expr         = make_bin(BIN_GE, expr, rhs);
        } else {
            break;
        }
    }

    return expr;
}

static AstNode *parse_equality(Parser *p) {
    AstNode *expr = parse_relational(p);

    for (;;) {
        if (p->cur.kind == TOK_EQ_EQ) {
            advance(p);
            AstNode *rhs = parse_relational(p);
            expr         = make_bin(BIN_EQ, expr, rhs);
        } else if (p->cur.kind == TOK_BANG_EQ) {
            advance(p);
            AstNode *rhs = parse_relational(p);
            expr         = make_bin(BIN_NE, expr, rhs);
        } else {
            break;
        }
    }

    return expr;
}

// ----- Bitwise expressions -----
static AstNode *parse_bitand(Parser *p) {
    AstNode *expr = parse_equality(p);

    while (p->cur.kind == TOK_AMP) {
        advance(p);
        AstNode *rhs = parse_equality(p);
        expr         = make_bin(BIN_BIT_AND, expr, rhs);
    }

    return expr;
}

static AstNode *parse_bitxor(Parser *p) {
    AstNode *expr = parse_bitand(p);

    while (p->cur.kind == TOK_CARET) {
        advance(p);
        AstNode *rhs = parse_bitand(p);
        expr         = make_bin(BIN_BIT_XOR, expr, rhs);
    }

    return expr;
}

static AstNode *parse_bitor(Parser *p) {
    AstNode *expr = parse_bitxor(p);

    while (p->cur.kind == TOK_PIPE) {
        advance(p);
        AstNode *rhs = parse_bitxor(p);
        expr         = make_bin(BIN_BIT_OR, expr, rhs);
    }

    return expr;
}

// ----- Logical expressions -----
static AstNode *parse_logical_and(Parser *p) {
    AstNode *expr = parse_bitor(p);

    while (p->cur.kind == TOK_AMP_AMP || p->cur.kind == TOK_KW_AND) {
        advance(p);
        AstNode *rhs = parse_bitor(p);
        expr         = make_bin(BIN_AND, expr, rhs);
    }

    return expr;
}

static AstNode *parse_logical_or(Parser *p) {
    AstNode *expr = parse_logical_and(p);

    while (p->cur.kind == TOK_PIPE_PIPE || p->cur.kind == TOK_KW_OR) {
        advance(p);
        AstNode *rhs = parse_logical_and(p);
        expr         = make_bin(BIN_OR, expr, rhs);
    }

    return expr;
}

// ----- Assignment expressions -----
static AstNode *make_assign(AstAssignOp op, AstNode *lhs, AstNode *rhs) {
    AstNode *n            = new_node(AST_ASSIGN_EXPR);
    n->as.assign_expr.op  = op;
    n->as.assign_expr.lhs = lhs;
    n->as.assign_expr.rhs = rhs;
    return n;
}
static AstNode *parse_assignment(Parser *p) {
    AstNode *lhs = parse_logical_or(p);

    if (p->cur.kind == TOK_EQ) {
        advance(p);
        AstNode *rhs = parse_assignment(p);
        return make_assign(ASSIGN_EQ, lhs, rhs);
    }
    // TODO: Add more assignment operators

    return lhs;
}

// ----- Expressions -----
static AstNode *parse_expr(Parser *p) { return parse_assignment(p); }
