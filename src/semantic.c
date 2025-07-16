#include "semantic.h"

#include "symtab.h"

// Entry point
void semantic_check(ASTNode *program) {
    SemanticContext ctx = {.return_type = NULL};

    check_node(program, &ctx);
}

static void report_error(int line, int col, const char *msg) {
    write(STDOUT_FILENO, "Semantic error: ", 16);
    write(STDOUT_FILENO, msg, str_len(msg));
    write(STDOUT_FILENO, " @ ", 3);

    char buf[32];
    int len = itoa64(line, buf);
    write(STDOUT_FILENO, buf, len);
    write(STDOUT_FILENO, ":", 1);
    len = itoa64(col, buf);
    write(STDOUT_FILENO, buf, len);
    write(STDOUT_FILENO, "\n", 1);
}

static void check_node(ASTNode *node, SemanticContext *ctx) {
    if (!node) return;

    switch (node->kind) {
        case AST_FUNCTION_DEF:
            check_function_def(node, ctx);
            break;

        case AST_BLOCK:
            if (!(node->parent && node->parent->kind == AST_FUNCTION_DEF)) {
                symtab_push_scope();
            }
            break;

        case AST_CONST_DECL:
        case AST_MUT_DECL:
            check_decl(node, ctx);
            break;

        case AST_RETURN_STMT:
            check_return(node, ctx);
            break;

        case AST_ASSIGN_STMT:
            check_assign(node, ctx);
            break;

        case AST_EXPR_STMT:
            check_expr_stmt(node, ctx);
            break;

        default:
            break;
    }

    for (ASTNode *c = node->first_child; c; c = c->next_sibling) {
        check_node(c, ctx);
    }
}

static void check_function_def(ASTNode *fn, SemanticContext *ctx) {
    ASTNode *return_type = fn->first_child;
    ASTNode *name = return_type->next_sibling;
    ASTNode *param_list = name->next_sibling;

    // Check if return type is valid, and store it in ctx
    if (!symtab_is_type(return_type->data.text.name)) {
        report_error(return_type->line, return_type->column,
                     "Unknown return type");
    }
    ctx->return_type = return_type;

    symtab_push_scope();  // Create new scope for function

    // Check parameters and add to scope.
    for (ASTNode *param = param_list->first_child; param;
         param = param->next_sibling) {
        ASTNode *type = param->first_child;
        ASTNode *name = type->next_sibling;

        if (!symtab_is_type(type->data.text.name)) {
            report_error(type->line, type->column, "Unknown parameter type");
        }

        if (!symtab_add_var(name->data.text.name, type, 0)) {
            report_error(name->line, name->column, "Duplicate parameter name");
        }
    }
}

static void check_decl(ASTNode *decl, SemanticContext *ctx) {
    ASTNode *type = decl->first_child;
    ASTNode *name = type->next_sibling;
    ASTNode *init = name->next_sibling;  // Can be NULL

    if (!symtab_is_type(type->data.text.name)) {
        report_error(type->line, type->column, "Unknown variable type");
    }

    int is_mut = (decl->kind == AST_MUT_DECL);
    if (!symtab_add_var(name->data.text.name, type, is_mut)) {
        report_error(name->line, name->column, "Duplicate variable definition");
    }

    if (init) {
        ctx->expected_int_lit_type = type;
        ASTNode *init_ty = check_expr(init, ctx);
        ctx->expected_int_lit_type = NULL;
        if (init_ty &&
            !symtab_same_type(init_ty->data.text.name, type->data.text.name)) {
            report_error(init->line, init->column, "Initializer type mismatch");
        }
    }
}

static void check_return(ASTNode *ret, SemanticContext *ctx) {
    ASTNode *type = check_expr(ret->first_child, ctx);
    if (!symtab_same_type(type->data.text.name,
                          ctx->return_type->data.text.name)) {
        report_error(ret->line, ret->column, "Return type mismatch");
    }
    ret->type = type;
    ctx->return_type = NULL;
}

static void check_expr_stmt(ASTNode *expr_stmt, SemanticContext *ctx) {
    check_expr(expr_stmt->first_child, ctx);
}

static void check_assign(ASTNode *assign, SemanticContext *ctx) {
    ASTNode *ident = assign->first_child;
    ASTNode *expr = ident->next_sibling;

    ASTNode *ident_type = symtab_find_var(ident->data.text.name);
    if (!ident_type) {
        report_error(ident->line, ident->column, "Undeclared identifier");
        return;
    }

    if (!symtab_is_mut(ident->data.text.name)) {
        report_error(ident->line, ident->column,
                     "Attempt to assign to non-mut variable");
        return;
    }

    ctx->expected_int_lit_type = ident_type;
    ASTNode *expr_type = check_expr(expr, ctx);
    ctx->expected_int_lit_type = NULL;
    if (expr_type && !symtab_same_type(expr_type->data.text.name,
                                       ident_type->data.text.name)) {
        report_error(ident->line, ident->column, "Type mismatch in assignment");
    }
}

/* Check expression for semantic errors, returns type of expression */
static ASTNode *check_expr(ASTNode *expr, SemanticContext *ctx) {
    switch (expr->first_child->kind) {
        case AST_ADD_EXPR: {
            ASTNode *type = check_add_expr(expr->first_child, ctx);
            expr->type = type;
            return type;
        }
        case AST_INT_LITERAL:
        case AST_IDENTIFIER:
        case AST_CALL_EXPR:
        case AST_EXPRESSION: {
            ASTNode *type = check_primary(expr->first_child, ctx);
            expr->type = type;
            return type;
        }
        default:
            break;
    }

    return NULL;
}

static ASTNode *check_add_expr(ASTNode *add_expr, SemanticContext *ctx) {
    ASTNode *lh = add_expr->first_child;
    ASTNode *rh = lh->next_sibling;

    ASTNode *lh_type = NULL;
    if (lh->kind == AST_ADD_EXPR) {
        lh_type = check_add_expr(lh, ctx);
    } else {
        lh_type = check_primary(lh, ctx);
    }

    ASTNode *rh_type = NULL;
    if (rh->kind == AST_ADD_EXPR) {
        rh_type = check_add_expr(rh, ctx);
    } else {
        rh_type = check_primary(rh, ctx);
    }

    if (!symtab_same_type(lh_type->data.text.name, rh_type->data.text.name)) {
        report_error(add_expr->line, add_expr->column,
                     "Type mismatch in add expression");
    }

    add_expr->type = lh_type;
    return lh_type;
}

static ASTNode *check_primary(ASTNode *primary, SemanticContext *ctx) {
    switch (primary->kind) {
        case AST_INT_LITERAL:
            if (primary->data.int_lit.type) {
                return primary->data.int_lit.type;
            }

            if (ctx->expected_int_lit_type) {
                if (symtab_same_type(ctx->expected_int_lit_type->data.text.name,
                                     "u8") &&
                    (primary->data.int_lit.i_val > UINT8_MAX ||
                     primary->data.int_lit.i_val < 0)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for u8 var");
                } else if (symtab_same_type(
                               ctx->expected_int_lit_type->data.text.name,
                               "u16") &&
                           (primary->data.int_lit.i_val > UINT16_MAX ||
                            primary->data.int_lit.i_val < 0)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for u16 var");
                } else if (symtab_same_type(
                               ctx->expected_int_lit_type->data.text.name,
                               "u32") &&
                           (primary->data.int_lit.i_val > UINT32_MAX ||
                            primary->data.int_lit.i_val < 0)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for u32 var");
                } else if (symtab_same_type(
                               ctx->expected_int_lit_type->data.text.name,
                               "u64") &&
                           (primary->data.int_lit.i_val > UINT64_MAX ||
                            primary->data.int_lit.i_val < 0)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for u64 var");
                } else if (symtab_same_type(
                               ctx->expected_int_lit_type->data.text.name,
                               "i8") &&
                           (primary->data.int_lit.i_val > INT8_MAX ||
                            primary->data.int_lit.i_val < INT8_MIN)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for i8 var");
                } else if (symtab_same_type(
                               ctx->expected_int_lit_type->data.text.name,
                               "i16") &&
                           (primary->data.int_lit.i_val > INT16_MAX ||
                            primary->data.int_lit.i_val < INT16_MIN)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for i16 var");
                } else if (symtab_same_type(
                               ctx->expected_int_lit_type->data.text.name,
                               "i32") &&
                           (primary->data.int_lit.i_val > INT32_MAX ||
                            primary->data.int_lit.i_val < INT32_MIN)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for i32 var");
                } else if (symtab_same_type(
                               ctx->expected_int_lit_type->data.text.name,
                               "i64") &&
                           (primary->data.int_lit.i_val > INT64_MAX ||
                            primary->data.int_lit.i_val < INT64_MIN)) {
                    report_error(primary->line, primary->column,
                                 "Number invalid for i64 var");
                }

                primary->data.int_lit.type = ctx->expected_int_lit_type;
                return primary->data.int_lit.type;
            } else {
                const char *type_name = "";
                if (primary->data.int_lit.i_val <= INT32_MAX &&
                    primary->data.int_lit.i_val >= INT32_MIN) {
                    type_name = "i32";
                } else if (primary->data.int_lit.i_val <= INT64_MAX &&
                           primary->data.int_lit.i_val >= INT64_MIN) {
                    type_name = "i64";
                } else {
                    report_error(primary->line, primary->column,
                                 "Number too large for i64");
                }

                Token ty_tok;
                ty_tok.line = primary->line;
                ty_tok.column = primary->column;
                ty_tok.type = TOKEN_IDENTIFIER;
                ty_tok.length = str_len(type_name);
                if (ty_tok.length > MAX_LEXEME) ty_tok.length = MAX_LEXEME;

                for (int i = 0; i < ty_tok.length; i++) {
                    ty_tok.lexeme[i] = type_name[i];
                }
                ty_tok.lexeme[ty_tok.length] = '\0';
                primary->data.int_lit.type = ast_make_type_name(ty_tok);
            }

            return primary->data.int_lit.type;

        case AST_IDENTIFIER: {
            ASTNode *ty = symtab_find_var(primary->data.text.name);
            if (!ty) {
                report_error(primary->line, primary->column,
                             "Use of undeclared variable");
                return NULL;
            }
            return ty;
        }
        case AST_CALL_EXPR: {
            ASTNode *type = check_call_expr(primary, ctx);
            primary->type = type;
            return type;
        }

        case AST_EXPRESSION: {
            ASTNode *type = check_expr(primary, ctx);
            primary->type = type;
            return type;
        }

        default:
            break;
    }
    return NULL;
}

static ASTNode *check_call_expr(ASTNode *call_expr, SemanticContext *ctx) {
    ASTNode *ident = call_expr->first_child;
    ASTNode *arg_list = ident->next_sibling;

    ASTNode *fn = symtab_find_function(ident->data.text.name);

    if (!fn) {
        report_error(ident->line, ident->column, "Call to undefined function");
        return NULL;
    }

    ASTNode *fn_arg_list = fn->first_child->next_sibling->next_sibling;
    int fn_arg_count = 0;
    for (ASTNode *arg = fn_arg_list->first_child; arg;
         arg = arg->next_sibling) {
        fn_arg_count++;
    }

    int arg_count = 0;
    for (ASTNode *arg = arg_list->first_child; arg; arg = arg->next_sibling) {
        arg_count++;
    }

    if (fn_arg_count != arg_count) {
        report_error(arg_list->line, arg_list->column,
                     "Argument count mismatch");
    }

    ASTNode *arg = arg_list->first_child;
    ASTNode *fn_arg = fn_arg_list->first_child;
    for (int i = 0; i < fn_arg_count; i++) {
        ASTNode *fn_arg_type = fn_arg->first_child;
        ctx->expected_int_lit_type = fn_arg_type;
        ASTNode *arg_type = check_expr(arg, ctx);
        ctx->expected_int_lit_type = NULL;
        if (!symtab_same_type(arg_type->data.text.name,
                              fn_arg_type->data.text.name)) {
            report_error(arg->line, arg->column,
                         "Argument type mismatch in function call");
        }

        arg = arg->next_sibling;
        fn_arg = fn_arg->next_sibling;
    }

    call_expr->type = fn->first_child;
    return fn->first_child;
}
