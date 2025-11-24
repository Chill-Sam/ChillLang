#include "lower_ir.h"
#include "ast.h"
#include "ir_builder.h"
#include <stdlib.h>
#include <string.h>

static AstNode *g_translation_unit = NULL;

typedef struct LowerVar {
    char *name;
    TypeId type;
    IrValue value;
} LowerVar;

typedef struct LowerScope {
    struct LowerScope *parent;
    LowerVar *vars;
    uint32_t count;
    uint32_t cap;
} LowerScope;

static void *xrealloc(void *ptr, size_t size, const char *where) {
    void *p = realloc(ptr, size);
    if (!p) {
        fprintf(stderr, "fatal: out of memory in %s\n", where);
        abort();
    }
    return p;
}

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

static LowerScope *lscope_create(LowerScope *parent) {
    LowerScope *s = malloc(sizeof *s);
    if (!s) {
        fprintf(stderr, "fatal: out of memory in lscope_create\n");
        abort();
    }
    s->parent = parent;
    s->vars   = NULL;
    s->count  = 0;
    s->cap    = 0;
    return s;
}

static int lw_type_width(TypeId t) {
    const Type *ty = type_get(t);
    return ty->bit_width;
}

static int lw_type_is_signed(TypeId t) {
    const Type *ty = type_get(t);
    return !ty->is_unsigned;
}

static void lscope_free(LowerScope *s) {
    if (!s)
        return;
    for (uint32_t i = 0; i < s->count; i++) {
        free(s->vars[i].name);
    }
    free(s->vars);
    free(s);
}

static void lscope_add_var(LowerScope *s, const char *name, TypeId type,
                           IrValue value) {
    if (s->count == s->cap) {
        uint32_t new_cap = s->cap ? s->cap * 2 : 8;
        s->vars =
            xrealloc(s->vars, new_cap * sizeof(LowerVar), "lscope_add_var");
        s->cap = new_cap;
    }
    s->vars[s->count].name  = strdup(name);
    s->vars[s->count].type  = type;
    s->vars[s->count].value = value;
    s->count++;
}

static LowerVar *lscope_lookup(LowerScope *s, const char *name) {
    for (LowerScope *cur = s; cur; cur = cur->parent) {
        for (uint32_t i = 0; i < cur->count; i++) {
            if (strcmp(cur->vars[i].name, name) == 0)
                return &cur->vars[i];
        }
    }
    return NULL;
}

static int token_eq_str(const Token *tok, const char *s) {
    size_t len = strlen(s);
    if (tok->length != len)
        return 0;
    return memcmp(tok->lexeme, s, len) == 0;
}

static TypeId lower_resolve_builtin_type(const Token *name_tok) {
    if (token_eq_str(name_tok, "void"))
        return TYPEID_VOID;
    if (token_eq_str(name_tok, "bool"))
        return TYPEID_BOOL;
    if (token_eq_str(name_tok, "i8"))
        return TYPEID_I8;
    if (token_eq_str(name_tok, "u8"))
        return TYPEID_U8;
    if (token_eq_str(name_tok, "i16"))
        return TYPEID_I16;
    if (token_eq_str(name_tok, "u16"))
        return TYPEID_U16;
    if (token_eq_str(name_tok, "i32"))
        return TYPEID_I32;
    if (token_eq_str(name_tok, "u32"))
        return TYPEID_U32;
    if (token_eq_str(name_tok, "i64"))
        return TYPEID_I64;
    if (token_eq_str(name_tok, "u64"))
        return TYPEID_U64;

    fprintf(stderr, "lowering error: unsupported type '%.*s' for IR\n",
            (int)name_tok->length, name_tok->lexeme);
    abort();
}

static AstNode *find_func_decl_by_name(const char *name) {
    AstNodeList *items = &g_translation_unit->as.translation_unit.items;
    for (uint32_t i = 0; i < items->count; i++) {
        AstNode *it = items->items[i];
        if (it->kind != AST_FUNC_DECL)
            continue;

        Token *tok = &it->as.func_decl.name;
        if (tok->length == strlen(name) &&
            memcmp(tok->lexeme, name, tok->length) == 0) {
            return it;
        }
    }
    return NULL;
}

static TypeId get_func_ret_type(const char *name) {
    AstNode *fn = find_func_decl_by_name(name);
    if (!fn) {
        fprintf(stderr, "lowering error: call to unknown function '%s'\n",
                name);
        abort();
    }

    AstFuncDecl *fd = &fn->as.func_decl;

    if (!fd->return_type) {
        return TYPEID_VOID;
    }
    if (fd->return_type->kind != AST_IDENT_EXPR) {
        fprintf(stderr, "lowering error: unsupported return type in call\n");
        abort();
    }

    Token *rtok = &fd->return_type->as.ident_expr.name;
    return lower_resolve_builtin_type(rtok);
}

static IrValue lower_expr(IrBuilder *b, LowerScope *scope, AstNode *expr,
                          TypeId *out_type);
static void lower_stmt(IrBuilder *b, LowerScope *scope, AstNode *stmt);
static void lower_block(IrBuilder *b, LowerScope *parent_scope, AstNode *block);
static IrFunc lower_func(AstNode *fn);

static IrValue lower_ident(IrBuilder *b, LowerScope *scope, AstNode *expr,
                           TypeId *out_type) {
    (void)b;
    Token *name_tok = &expr->as.ident_expr.name;
    char *name      = token_to_cstr(name_tok);
    LowerVar *lv    = lscope_lookup(scope, name);
    free(name);

    if (!lv) {
        fprintf(stderr, "lowering error: undefined variable '%.*s'\n",
                (int)name_tok->length, name_tok->lexeme);
        abort();
    }

    if (out_type)
        *out_type = lv->type;
    return lv->value;
}

static int64_t parse_int_literal(const Token *tok) {
    // NOTE:: assume base-10, ignore suffix/base fields for now.
    char *buf = malloc(tok->length + 1);
    if (!buf) {
        fprintf(stderr, "fatal: out of memory in parse_int_literal\n");
        abort();
    }
    memcpy(buf, tok->lexeme, tok->length);
    buf[tok->length] = '\0';
    char *end        = NULL;
    int64_t val      = strtoll(buf, &end, 0);
    free(buf);
    return val;
}

static IrValue lower_literal(IrBuilder *b, AstNode *expr, TypeId *out_type) {
    Token *tok = &expr->as.literal_expr.tok;
    // TODO: Allow other literal types
    if (tok->kind != TOK_INT_LITERAL && tok->kind != TOK_KW_TRUE &&
        tok->kind != TOK_KW_FALSE) {
        fprintf(stderr, "lowering error: only int literals and bool literals "
                        "supported in IR for now\n");
        abort();
    }

    // Bool literal
    if (tok->kind == TOK_KW_TRUE || tok->kind == TOK_KW_FALSE) {
        IrValue dst = irb_new_value(b, TYPEID_BOOL);

        IrInst inst;
        inst.op   = IR_CONST_INT;
        inst.type = TYPEID_BOOL;
        inst.dst  = dst;
        inst.imm  = tok->kind == TOK_KW_TRUE ? 1 : 0;
        irb_emit(b, inst);

        if (out_type)
            *out_type = TYPEID_BOOL;
        return dst;
    }

    // Int literal
    if (tok->kind == TOK_INT_LITERAL) {
        // Default to i32 for now
        TypeId t = TYPEID_I32;
        switch (tok->lit.int_literal.suffix) {
        case INT_SUFFIX_I8:
            t = TYPEID_I8;
            break;
        case INT_SUFFIX_I16:
            t = TYPEID_I16;
            break;
        case INT_SUFFIX_I32:
            t = TYPEID_I32;
            break;
        case INT_SUFFIX_I64:
            t = TYPEID_I64;
            break;
        case INT_SUFFIX_U8:
            t = TYPEID_U8;
            break;
        case INT_SUFFIX_U16:
            t = TYPEID_U16;
            break;
        case INT_SUFFIX_U32:
            t = TYPEID_U32;
            break;
        case INT_SUFFIX_U64:
            t = TYPEID_U64;
            break;
        default:
            break;
        }

        if (out_type)
            *out_type = t;

        int64_t imm = parse_int_literal(tok);
        return irb_const_int(b, t, imm);
    }

    return (IrValue)~0u; // unreachable
}

static IrValue lower_bin_expr(IrBuilder *b, LowerScope *scope, AstNode *expr,
                              TypeId *out_type) {
    AstBinExpr *bin = &expr->as.bin_expr;

    TypeId lt, rt;
    IrValue lhs = lower_expr(b, scope, bin->lhs, &lt);
    IrValue rhs = lower_expr(b, scope, bin->rhs, &rt);

    if (lt != rt) {
        fprintf(stderr,
                "lowering error: mismatched types in binary expression\n");
        abort();
    }

    TypeId t = lt;
    if (out_type)
        *out_type = t;

    // TODO: Handle all binary operators
    IrOp op;
    switch (bin->op) {
    case BIN_ADD:
        op = IR_OP_ADD;
        break;
    case BIN_SUB:
        op = IR_OP_SUB;
        break;
    case BIN_MUL:
        op = IR_OP_MUL;
        break;
    case BIN_DIV:
        op = IR_OP_DIV;
        break;
    case BIN_MOD:
        op = IR_OP_MOD;
        break;
    case BIN_BIT_AND:
        op = IR_OP_AND;
        break;
    case BIN_BIT_OR:
        op = IR_OP_OR;
        break;
    case BIN_BIT_XOR:
        op = IR_OP_XOR;
        break;
    case BIN_SHL:
        op = IR_OP_SHL;
        break;
    case BIN_SHR:
        op = IR_OP_SHR;
        break;
    case BIN_LT:
        op = IR_OP_CMP_LT;
        break;
    case BIN_GT:
        op = IR_OP_CMP_GT;
        break;
    case BIN_LE:
        op = IR_OP_CMP_LE;
        break;
    case BIN_GE:
        op = IR_OP_CMP_GE;
        break;
    case BIN_EQ:
        op = IR_OP_CMP_EQ;
        break;
    case BIN_NE:
        op = IR_OP_CMP_NE;
        break;
    default:
        fprintf(stderr, "lowering error: unsupported binary operator\n");
        abort();
    }

    return irb_binop(b, op, t, lhs, rhs);
}

static IrValue lower_unary_expr(IrBuilder *b, LowerScope *scope, AstNode *expr,
                                TypeId *out_type) {
    AstUnaryExpr *un = &expr->as.unary_expr;

    TypeId t;
    IrValue src = lower_expr(b, scope, un->expr, &t);

    TypeId t2   = t;
    if (out_type)
        *out_type = t2;

    IrOp op;
    switch (un->op) {
    case UN_NEG:
        op = IR_OP_NEG;
        break;
    case UN_NOT:
        op = IR_OP_NOT;
        break;
    case UN_BITNOT:
        op = IR_OP_BITNOT;
        break;
    default:
        fprintf(stderr, "lowering error: unsupported unary operator\n");
        abort();
    }

    return irb_unop(b, op, t2, src);
}

static IrValue lower_call_expr(IrBuilder *b, LowerScope *scope, AstNode *expr,
                               TypeId *out_type) {
    AstCallExpr *call = &expr->as.call_expr;

    if (call->callee->kind != AST_IDENT_EXPR) {
        fprintf(stderr, "lowering error: only simple function calls supported "
                        "in IR for now\n");
        abort();
    }

    Token *name_tok = &call->callee->as.ident_expr.name;
    char *name      = token_to_cstr(name_tok);

    IrValue arg_vals[6];
    uint32_t arg_count = 0;

    AstNodeList *args  = &call->args;
    if (args->count > 6) {
        fprintf(stderr,
                "lowering error: more than 6 call args not supported\n");
        abort();
    }

    for (uint32_t i = 0; i < args->count; i++) {
        TypeId arg_type;
        IrValue arg_val       = lower_expr(b, scope, args->items[i], &arg_type);
        arg_vals[arg_count++] = arg_val;
        (void)arg_type;
    }

    TypeId ret_type = get_func_ret_type(name);
    if (out_type)
        *out_type = ret_type;

    IrValue result = irb_call(b, ret_type, name, arg_count, arg_vals);

    free(name);
    return result;
}

static IrValue lower_assign_expr(IrBuilder *b, LowerScope *scope, AstNode *expr,
                                 TypeId *out_type) {
    AstAssignExpr *assign = &expr->as.assign_expr;

    if (assign->lhs->kind != AST_IDENT_EXPR) {
        fprintf(stderr, "lowering error: only assignment to variables "
                        "supported in IR for now\n");
        abort();
    }

    Token *name_tok = &assign->lhs->as.ident_expr.name;
    char *name      = token_to_cstr(name_tok);
    LowerVar *lv    = lscope_lookup(scope, name);
    free(name);

    if (!lv) {
        fprintf(stderr, "lowering error: undefined variable '%.*s'\n",
                (int)name_tok->length, name_tok->lexeme);
        abort();
    }

    TypeId rhs_t;
    IrValue rhs = lower_expr(b, scope, assign->rhs, &rhs_t);

    if (lv->type != rhs_t) {
        fprintf(stderr, "lowering error: mismatched types in assignment\n");
        abort();
    }

    lv->value = rhs;

    if (out_type)
        *out_type = lv->type;
    return lv->value;
}

static IrValue lower_cast_expr(IrBuilder *b, LowerScope *scope, AstNode *expr,
                               TypeId *out_type) {
    AstCastExpr *cast = &expr->as.cast_expr;

    TypeId src_t;
    IrValue src  = lower_expr(b, scope, cast->expr, &src_t);

    TypeId dst_t = cast->target_type;

    if (src_t == dst_t) {
        if (out_type)
            *out_type = dst_t;
        return src;
    }

    int src_w      = lw_type_width(src_t);
    int dst_w      = lw_type_width(dst_t);
    int src_signed = lw_type_is_signed(src_t);
    int dst_signed = lw_type_is_signed(dst_t);

    if (src_w < dst_w) {
        IrOp op   = src_signed ? IR_OP_SEXT : IR_OP_ZEXT;
        IrValue v = irb_unary_cast(b, op, dst_t, src);
        if (out_type)
            *out_type = dst_t;
        return v;
    } else if (src_w > dst_w) {
        IrValue v = irb_unary_cast(b, IR_OP_TRUNC, dst_t, src);
        if (out_type)
            *out_type = dst_t;
        return v;
    } else {
        // Same-width, reinterpret as new signedness
        if (out_type)
            *out_type = dst_t;
        return src;
    }
}

static IrValue lower_expr(IrBuilder *b, LowerScope *scope, AstNode *expr,
                          TypeId *out_type) {
    switch (expr->kind) {
    case AST_IDENT_EXPR:
        return lower_ident(b, scope, expr, out_type);

    case AST_LITERAL_EXPR:
        return lower_literal(b, expr, out_type);

    case AST_BIN_EXPR:
        return lower_bin_expr(b, scope, expr, out_type);

    case AST_UNARY_EXPR:
        return lower_unary_expr(b, scope, expr, out_type);

    case AST_CALL_EXPR:
        return lower_call_expr(b, scope, expr, out_type);

    case AST_ASSIGN_EXPR:
        return lower_assign_expr(b, scope, expr, out_type);

    case AST_CAST_EXPR:
        return lower_cast_expr(b, scope, expr, out_type);

    default:
        // TODO: Handle other expr kinds, e.g. call expr and member expr
        fprintf(stderr, "lowering error: unsupported expr kind %d\n",
                (int)expr->kind);
        abort();
    }
}

static void lower_var_decl(IrBuilder *b, LowerScope *scope, AstNode *stmt) {
    AstVarDecl *var = &stmt->as.var_decl;

    // TODO: Member access ??
    if (var->type->kind != AST_IDENT_EXPR) {
        fprintf(stderr,
                "lowering error: only basic ident types supported in IR for "
                "now\n");
        abort();
    }

    Token *type_tok = &var->type->as.ident_expr.name;
    TypeId type     = lower_resolve_builtin_type(type_tok);
    char *name      = token_to_cstr(&var->name);

    IrValue v;

    if (var->init) {
        TypeId t;
        IrValue init = lower_expr(b, scope, var->init, &t);

        if (t != type) {
            fprintf(stderr, "lowering error: mismatched types in var init\n");
            abort();
        }

        v = init;
    } else {
        v = irb_new_value(b, type);
    }

    lscope_add_var(scope, name, type, v);
    free(name);
}

static void lower_return(IrBuilder *b, LowerScope *scope, AstNode *stmt) {
    if (!stmt->as.return_stmt.expr) {
        irb_ret_void(b);
        return;
    }

    TypeId t;
    IrValue v = lower_expr(b, scope, stmt->as.return_stmt.expr, &t);

    irb_ret(b, v, t);
}

static void lower_expr_stmt(IrBuilder *b, LowerScope *scope, AstNode *stmt) {
    if (!stmt->as.expr_stmt.expr)
        return;
    (void)lower_expr(b, scope, stmt->as.expr_stmt.expr, NULL);
}

static void lower_stmt(IrBuilder *b, LowerScope *scope, AstNode *stmt) {
    switch (stmt->kind) {
    case AST_VAR_DECL:
        lower_var_decl(b, scope, stmt);
        break;

    case AST_RETURN_STMT:
        lower_return(b, scope, stmt);
        break;

    case AST_EXPR_STMT:
        lower_expr_stmt(b, scope, stmt);
        break;

    case AST_BLOCK_STMT:
        lower_block(b, scope, stmt);
        break;

    default:
        fprintf(stderr, "lowering error: unsupported stmt kind %d\n",
                (int)stmt->kind);
        abort();
    }
}

static void lower_block(IrBuilder *b, LowerScope *parent_scope,
                        AstNode *block) {
    if (!block || block->kind != AST_BLOCK_STMT) {
        fprintf(stderr, "lowering error: expected block statement\n");
        abort();
    }

    LowerScope *scope  = lscope_create(parent_scope);

    AstNodeList *stmts = &block->as.block_stmt.stmts;
    for (uint32_t i = 0; i < stmts->count; i++) {
        lower_stmt(b, scope, stmts->items[i]);
    }

    lscope_free(scope);
}

static IrFunc lower_func(AstNode *fn) {
    AstFuncDecl *fd = &fn->as.func_decl;

    TypeId ret_type = TYPEID_VOID;
    if (fd->return_type) {
        if (fd->return_type->kind != AST_IDENT_EXPR) {
            fprintf(stderr, "lowering error: unsupported return type\n");
            abort();
        }

        Token *rtok = &fd->return_type->as.ident_expr.name;
        ret_type    = lower_resolve_builtin_type(rtok);
    }

    char *name   = token_to_cstr(&fd->name);
    IrFunc ir_fn = ir_func_create(name, ret_type, fd->params.count);
    IrBuilder b;
    ir_builder_init(&b, &ir_fn);

    LowerScope *scope = lscope_create(NULL);

    for (uint32_t i = 0; i < fd->params.count; i++) {
        AstNode *param_node = fd->params.items[i];
        AstParam *param     = &param_node->as.param;

        if (param->type->kind != AST_IDENT_EXPR) {
            fprintf(stderr, "lowering error: unsupported param type\n");
            abort();
        }

        Token *type_tok = &param->type->as.ident_expr.name;
        TypeId type     = lower_resolve_builtin_type(type_tok);

        char *pname     = token_to_cstr(&param->name);

        if (i >= ir_fn.value_count) {
            fprintf(stderr, "lowering error: too many params\n");
            abort();
        }

        ir_fn.value_types[i] = type;
        IrValue v            = (IrValue)i;
        lscope_add_var(scope, pname, type, v);
        free(pname);
    }

    lower_block(&b, scope, fd->body);

    lscope_free(scope);
    return ir_fn;
}

IrModule *lower_to_ir(AstNode *tu) {
    if (!tu || tu->kind != AST_TRANSLATION_UNIT) {
        fprintf(stderr, "lowering error: expected translation unit node\n");
        abort();
    }

    g_translation_unit = tu;

    IrModule *m        = malloc(sizeof *m);
    if (!m) {
        fprintf(stderr, "fatal: out of memory in lower_to_ir\n");
        abort();
    }

    ir_module_init(m);

    AstNodeList *items = &tu->as.translation_unit.items;
    for (uint32_t i = 0; i < items->count; i++) {
        AstNode *it = items->items[i];
        if (it->kind == AST_FUNC_DECL) {
            IrFunc fn = lower_func(it);
            ir_module_add_func(m, fn);
        } else {
            // TODO: structs
        }
    }

    return m;
}
