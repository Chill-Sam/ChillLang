#include "semantic.h"

#include "parser.h"
#include "symtab.h"
#include "token.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Context {
    TypeId return_type;
} Context;

static Scope *g_global_scope = NULL;
static Context ctx;

static char *token_to_cstr(const Token *tok) {
    char *s = malloc(tok->length + 1);
    if (!s) {
        fprintf(stderr, "fatal: out of memory in token_to_cstr\n");
        abort();
    }
    memcpy(s, tok->lexeme, tok->length);
    s[tok->length] = '\0';
    return s;
}

// TODO: Replace with proper diagnostics system
static void sema_fatal(const Token *where, const char *msg) {
    if (where) {
        fprintf(stderr, "sema error at %u:%u: %s (near '%.*s')\n", where->line,
                where->column, msg, (int)where->length, where->lexeme);
    } else {
        fprintf(stderr, "sema error: %s\n", msg);
    }
    exit(1);
}

static AstNode *sema_insert_cast(AstNode *expr, TypeId target) {
    if (expr->type_id == target)
        return expr;

    AstNode *cast                  = new_node(AST_CAST_EXPR);
    cast->as.cast_expr.expr        = expr;
    cast->as.cast_expr.target_type = target;
    cast->type_id                  = target;
    return cast;
}

static void sema_init_builtin_types(Scope *global) {
    types_init();

    struct Builtin {
        const char *name;
        TypeId id;
    } builtins[] = {
        {"void", TYPEID_VOID}, {"bool", TYPEID_BOOL}, {"i8", TYPEID_I8},
        {"u8", TYPEID_U8},     {"i16", TYPEID_I16},   {"u16", TYPEID_U16},
        {"i32", TYPEID_I32},   {"u32", TYPEID_U32},   {"i64", TYPEID_I64},
        {"u64", TYPEID_U64},   {"f32", TYPEID_F32},   {"f64", TYPEID_F64},
    };

    for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); i++) {
        if (!scope_define_type(global, builtins[i].name, builtins[i].id)) {
            fprintf(stderr, "fatal: builtin type '%s' redeclared\n",
                    builtins[i].name);
            exit(1);
        }
    }
}

static TypeId sema_resolve_type_name(Scope *scope, AstNode *type_expr) {
    if (!type_expr || type_expr->kind != AST_IDENT_EXPR) {
        sema_fatal(NULL, "internal error: expected IDENT_EXPR as type name");
    }

    Token *name_tok = &type_expr->as.ident_expr.name;
    char *name      = token_to_cstr(name_tok);

    Symbol *sym     = scope_lookup(scope, name);
    free(name);

    if (!sym || sym->kind != SYM_TYPE) {
        sema_fatal(name_tok, "unknown type name");
    }

    return sym->as.type.type_id;
}

// First pass of semantic analysis
static void sema_pass1_collect_globals(AstNode *tu, Scope *global) {
    if (!tu || tu->kind != AST_TRANSLATION_UNIT) {
        sema_fatal(NULL, "internal error: expected TRANSLATION_UNIT");
    }

    AstNodeList *items = &tu->as.translation_unit.items;

    for (uint32_t i = 0; i < items->count; i++) {
        AstNode *item = items->items[i];
        switch (item->kind) {
        case AST_STRUCT_DECL: {
            Token *name_token = &item->as.struct_decl.name;
            char *name        = token_to_cstr(name_token);
            Type t            = type_make_struct(item);
            TypeId tid        = type_add(t);
            if (!scope_define_type(global, name, tid)) {
                free(name);
                sema_fatal(name_token, "redefinition of type");
            }
            free(name);
            break;
        }

        case AST_FUNC_DECL: {
            Token *name_token = &item->as.func_decl.name;
            char *name        = token_to_cstr(name_token);

            if (!scope_define_func(global, name, item)) {
                free(name);
                sema_fatal(name_token, "redefinition of function");
            }
            free(name);
            break;
        }

        default:
            sema_fatal(NULL, "internal error: unknown top-level AST node kind");
        }
    }
}

// Second pass of semantic analysis
static TypeId sema_expr(Scope *scope, AstNode *expr);
static void sema_stmt(Scope *scope, AstNode *stmt);
static void sema_block(Scope *parent_scope, AstNode *block);

static TypeId sema_expr_ident(Scope *scope, AstNode *expr) {
    Token *name_token = &expr->as.ident_expr.name;
    char *name        = token_to_cstr(name_token);
    Symbol *sym       = scope_lookup(scope, name);
    free(name);

    if (!sym) {
        sema_fatal(name_token, "use of undeclared identifier");
    }

    switch (sym->kind) {
    case SYM_VAR:
        expr->type_id = sym->as.var.type_id;
        return sym->as.var.type_id;
    case SYM_FUNC:
        // TODO: Maybe update to allow first-class functions
        sema_fatal(name_token, "functions are not first-class yet");
        break;
    case SYM_TYPE:
        sema_fatal(name_token, "type name used as value");
        break;
    }
    sema_fatal(name_token, "internal error: unknown symbol kind");
    return TYPEID_VOID; // unreachable
}

// NOTE: Temporary implementation
static TypeId sema_expr_literal(AstNode *expr) {
    Token *tok = &expr->as.literal_expr.tok;
    switch (tok->kind) {
    case TOK_INT_LITERAL: {
        IntLiteralInfo info = tok->lit.int_literal;
        switch (info.suffix) {
        case INT_SUFFIX_I8:
            expr->type_id = TYPEID_I8;
            return TYPEID_I8;
        case INT_SUFFIX_I16:
            expr->type_id = TYPEID_I16;
            return TYPEID_I16;
        case INT_SUFFIX_I32:
            expr->type_id = TYPEID_I32;
            return TYPEID_I32;
        case INT_SUFFIX_I64:
            expr->type_id = TYPEID_I64;
            return TYPEID_I64;
        case INT_SUFFIX_U8:
            expr->type_id = TYPEID_U8;
            return TYPEID_U8;
        case INT_SUFFIX_U16:
            expr->type_id = TYPEID_U16;
            return TYPEID_U16;
        case INT_SUFFIX_U32:
            expr->type_id = TYPEID_U32;
            return TYPEID_U32;
        case INT_SUFFIX_U64:
            expr->type_id = TYPEID_U64;
            return TYPEID_U64;
        case INT_SUFFIX_NONE:
        default:
            // Default integer type: i32
            expr->type_id = TYPEID_I32;
            return TYPEID_I32;
        }
    }

    case TOK_FLOAT_LITERAL: {
        FloatLiteralInfo info = tok->lit.float_literal;
        switch (info.suffix) {
        case FLOAT_SUFFIX_F32:
            expr->type_id = TYPEID_F32;
            return TYPEID_F32;
        case FLOAT_SUFFIX_F64:
            expr->type_id = TYPEID_F64;

            return TYPEID_F64;
        case FLOAT_SUFFIX_NONE:
        default:
            // Default float type: f32
            expr->type_id = TYPEID_F32;
            return TYPEID_F32;
        }
    }

    case TOK_KW_TRUE:
    case TOK_KW_FALSE:
        expr->type_id = TYPEID_BOOL;
        return TYPEID_BOOL;

    case TOK_CHAR_LITERAL:
        // NOTE: Reusing I32 for now
        expr->type_id = TYPEID_I32;
        return TYPEID_I32;

    case TOK_STRING_LITERAL:
        // TODO: Provide proper pointer type
        expr->type_id = TYPEID_VOID;
        return TYPEID_VOID;

    default:
        sema_fatal(tok, "internal error: unknown literal kind");
    }

    return TYPEID_VOID; // unreachable
}

static TypeId sema_expr_member(Scope *scope, AstNode *expr) {
    AstMemberExpr *member = &expr->as.member_expr;
    TypeId base_type      = sema_expr(scope, member->base);
    const Type *base_t    = type_get(base_type);

    if (base_t->kind != TYPE_STRUCT) {
        sema_fatal(&member->field, "member access on non-struct type");
    }

    AstNode *struct_decl = base_t->struct_decl;
    AstNodeList *fields  = &struct_decl->as.struct_decl.fields;
    Token *field_tok     = &expr->as.member_expr.field;

    for (uint32_t i = 0; i < fields->count; i++) {
        AstNode *field  = fields->items[i];
        Token *name_tok = &field->as.field.name;
        if (name_tok->length == field_tok->length &&
            memcmp(name_tok->lexeme, field_tok->lexeme, field_tok->length) ==
                0) {
            TypeId t =
                sema_resolve_type_name(g_global_scope, field->as.field.type);
            expr->type_id = t;
            return t;
        }
    }

    sema_fatal(field_tok, "unknown field");
    return TYPEID_VOID; // unreachable
}

static TypeId sema_expr_assign(Scope *scope, AstNode *expr) {
    AstAssignExpr *assign = &expr->as.assign_expr;

    // TODO: Allow other types of assignment
    if (assign->op != ASSIGN_EQ) {
        sema_fatal(NULL, "internal error: unknown assignment operator");
    }

    bool lhs_mut    = false;
    TypeId lhs_type = TYPEID_VOID;

    if (assign->lhs->kind == AST_IDENT_EXPR) {
        Token *name_tok = &assign->lhs->as.ident_expr.name;
        char *name      = token_to_cstr(name_tok);
        Symbol *sym     = scope_lookup(scope, name);
        free(name);

        if (!sym) {
            sema_fatal(name_tok, "use of undeclared identifier");
        }

        if (sym->kind != SYM_VAR) {
            sema_fatal(name_tok,
                       "left-hand side of assignment is not a mutable var");
        }

        lhs_type = sym->as.var.type_id;
        lhs_mut  = sym->as.var.is_mut;
    } else if (assign->lhs->kind == AST_MEMBER_EXPR) {
        lhs_type = sema_expr_member(scope, assign->lhs);
        lhs_mut  = true;
    } else {
        sema_fatal(NULL,
                   "internal error: unknown left-hand side of assignment");
    }

    if (!lhs_mut) {
        sema_fatal(&assign->lhs->as.ident_expr.name,
                   "cannot assign to immutable");
    }

    TypeId rhs_type = sema_expr(scope, assign->rhs);

    if (!type_can_implicitly_convert(rhs_type, lhs_type)) {
        sema_fatal(&assign->rhs->as.ident_expr.name,
                   "type mismatch in assignment");
    }

    if (rhs_type != lhs_type) {
        assign->rhs = sema_insert_cast(assign->rhs, lhs_type);
    }

    expr->type_id = lhs_type;

    return lhs_type;
}

static TypeId sema_expr_bin(Scope *scope, AstNode *expr) {
    AstBinExpr *bin = &expr->as.bin_expr;
    TypeId lhs_type = sema_expr(scope, bin->lhs);
    TypeId rhs_type = sema_expr(scope, bin->rhs);

    TypeId result   = type_binary_result(lhs_type, rhs_type, expr);
    if (result == TYPEID_INVALID) {
        sema_fatal(NULL, "type mismatch in binary expression");
    }

    // Comparison operators
    if (type_is_bool(result)) {
        TypeId int_result = type_binary_int(lhs_type, rhs_type);
        if (int_result == TYPEID_INVALID) {
            sema_fatal(NULL, "type mismatch in comparison expression");
        }

        if (lhs_type != int_result) {
            bin->lhs = sema_insert_cast(bin->lhs, result);
        }

        if (rhs_type != int_result) {
            bin->rhs = sema_insert_cast(bin->rhs, result);
        }
    }

    // Arithmetic operators
    if (type_is_integer(result)) {
        if (lhs_type != result) {
            bin->lhs = sema_insert_cast(bin->lhs, result);
        }

        if (rhs_type != result) {
            bin->rhs = sema_insert_cast(bin->rhs, result);
        }
    }

    expr->type_id = result;
    return result;
}

static TypeId sema_expr_unary(Scope *scope, AstNode *expr) {
    AstUnaryExpr *unary = &expr->as.unary_expr;
    TypeId expr_type    = sema_expr(scope, unary->expr);

    expr->type_id       = expr_type;
    // TODO: allows any numeric type, refine later
    return expr_type;
}

static TypeId sema_expr_call(Scope *scope, AstNode *expr) {
    AstCallExpr *call = &expr->as.call_expr;

    if (call->callee->kind != AST_IDENT_EXPR) {
        sema_fatal(NULL, "only simple function calls are supported");
    }

    Token *name_tok = &call->callee->as.ident_expr.name;
    char *name      = token_to_cstr(name_tok);
    Symbol *sym     = scope_lookup(scope, name);
    free(name);

    if (!sym || sym->kind != SYM_FUNC) {
        sema_fatal(name_tok, "call to undeclared function");
    }

    AstNode *fn         = sym->as.func.func_decl;
    AstNodeList *params = &fn->as.func_decl.params;

    if (params->count != call->args.count) {
        sema_fatal(name_tok, "wrong number of arguments");
    }

    for (uint32_t i = 0; i < call->args.count; i++) {
        TypeId arg_type   = sema_expr(scope, call->args.items[i]);
        TypeId param_type = sema_resolve_type_name(
            g_global_scope, params->items[i]->as.param.type);

        if (!type_can_implicitly_convert(arg_type, param_type)) {
            sema_fatal(name_tok, "type mismatch in argument");
        }

        if (arg_type != param_type) {
            call->args.items[i] =
                sema_insert_cast(call->args.items[i], param_type);
        }
    }

    if (!fn->as.func_decl.return_type) {
        expr->type_id = TYPEID_VOID;
        return TYPEID_VOID;
    }

    TypeId t =
        sema_resolve_type_name(g_global_scope, fn->as.func_decl.return_type);
    expr->type_id = t;
    return t;
}

static TypeId sema_expr(Scope *scope, AstNode *expr) {
    switch (expr->kind) {
    case AST_IDENT_EXPR:
        return sema_expr_ident(scope, expr);

    case AST_LITERAL_EXPR:
        return sema_expr_literal(expr);

    case AST_MEMBER_EXPR:
        return sema_expr_member(scope, expr);

    case AST_ASSIGN_EXPR:
        return sema_expr_assign(scope, expr);

    case AST_BIN_EXPR:
        return sema_expr_bin(scope, expr);

    case AST_UNARY_EXPR:
        return sema_expr_unary(scope, expr);

    case AST_CALL_EXPR:
        return sema_expr_call(scope, expr);

    case AST_CAST_EXPR:
        sema_expr(scope, expr->as.cast_expr.expr);
        if (expr->as.cast_expr.target) {
            expr->as.cast_expr.target_type = sema_resolve_type_name(
                g_global_scope, expr->as.cast_expr.target);
        }
        expr->type_id = expr->as.cast_expr.target_type;
        return expr->type_id;

    default:
        sema_fatal(NULL, "internal error: unexpected expr node kind");
    }

    return TYPEID_VOID; // unreachable
}

static void sema_stmt(Scope *scope, AstNode *stmt) {
    switch (stmt->kind) {
    case AST_VAR_DECL: {
        AstVarDecl *var = &stmt->as.var_decl;
        TypeId type_id  = sema_resolve_type_name(scope, var->type);
        char *name      = token_to_cstr(&var->name);
        if (!scope_define_var(scope, name, type_id, var->is_mut, stmt)) {
            free(name);
            sema_fatal(&var->name, "redefinition of variable in same scope");
        }
        free(name);

        // Force initialization of immutable variables
        if (!var->is_mut && !var->init) {
            sema_fatal(&var->name, "uninitialized constant variable");
        }

        // Check if the variable is initialized
        if (var->init) {
            TypeId init_type = sema_expr(scope, var->init);
            if (!type_can_implicitly_convert(init_type, type_id)) {
                sema_fatal(&stmt->as.var_decl.name,
                           "type mismatch in variable initialization");
            }

            if (type_id != init_type) {
                var->init = sema_insert_cast(var->init, type_id);
            }
        }

        break;
    }

    case AST_IF_STMT: {
        if (stmt->as.if_stmt.cond) {
            TypeId cond_type = sema_expr(scope, stmt->as.if_stmt.cond);
            if (!type_is_bool(cond_type)) {
                sema_fatal(&stmt->as.if_stmt.cond->as.ident_expr.name,
                           "condition in if statement must be a boolean");
            }
        }

        if (stmt->as.if_stmt.then_block) {
            sema_block(scope, stmt->as.if_stmt.then_block);
        }

        if (stmt->as.if_stmt.else_block) {
            sema_block(scope, stmt->as.if_stmt.else_block);
        }
        break;
    }

    case AST_EXPR_STMT: {
        if (stmt->as.expr_stmt.expr) {
            (void)sema_expr(scope, stmt->as.expr_stmt.expr);
        }

        break;
    }

    case AST_RETURN_STMT:
        if (stmt->as.return_stmt.expr) {
            TypeId t = sema_expr(scope, stmt->as.return_stmt.expr);
            if (!type_can_implicitly_convert(t, ctx.return_type)) {
                sema_fatal(NULL, "mismatched types in return statement");
            }

            if (ctx.return_type != t) {
                stmt->as.return_stmt.expr = sema_insert_cast(
                    stmt->as.return_stmt.expr, ctx.return_type);
            }
        }
        break;

    case AST_BLOCK_STMT:
        sema_block(scope, stmt);
        break;

    default:
        sema_fatal(NULL, "internal error: unexpected stmt node kind");
    }
}

static void sema_block(Scope *parent_scope, AstNode *block) {
    if (!block || block->kind != AST_BLOCK_STMT) {
        sema_fatal(NULL, "internal error: expected BLOCK_STMT");
    }

    Scope *scope       = scope_create(parent_scope);

    AstNodeList *stmts = &block->as.block_stmt.stmts;
    for (uint32_t i = 0; i < stmts->count; i++) {
        sema_stmt(scope, stmts->items[i]);
    }

    scope_destroy(scope);
}

static void sema_func(AstNode *fn) {
    AstFuncDecl *func  = &fn->as.func_decl;

    TypeId return_type = TYPEID_VOID;
    if (func->return_type) {
        return_type = sema_resolve_type_name(g_global_scope, func->return_type);
    }
    ctx.return_type     = return_type;

    Scope *fn_scope     = scope_create(g_global_scope);

    AstNodeList *params = &func->params;
    for (uint32_t i = 0; i < params->count; i++) {
        AstParam *param   = &params->items[i]->as.param;
        TypeId param_type = sema_resolve_type_name(g_global_scope, param->type);
        char *name        = token_to_cstr(&param->name);
        if (!scope_define_var(fn_scope, name, param_type, param->is_mut,
                              params->items[i])) {
            free(name);
            sema_fatal(&param->name, "redefinition of parameter in same scope");
        }
        free(name);
    }

    sema_block(fn_scope, func->body);

    scope_destroy(fn_scope);
    (void)return_type;
}

static void sema_pass2_analyze_functions(AstNode *tu) {
    AstNodeList *items = &tu->as.translation_unit.items;

    for (uint32_t i = 0; i < items->count; i++) {
        AstNode *it = items->items[i];
        if (it->kind == AST_FUNC_DECL) {
            sema_func(it);
        }
    }
}

void sema_analyze(AstNode *tu) {
    g_global_scope = scope_create(NULL);
    sema_init_builtin_types(g_global_scope);

    sema_pass1_collect_globals(tu, g_global_scope);
    sema_pass2_analyze_functions(tu);
    // TODO: Add pass 3
}
