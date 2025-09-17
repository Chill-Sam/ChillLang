#include "irgen.h"

#include "AST/ast_arena.c"
#include "file_stream.c"
#include "ir.h"
#include "ir_type.c"
#include "symtab.h"

ASTNode *name_to_assign = NULL;

static char *make_name(char prefix) {
    char *buf = ast_arena_alloc(16);
    int n = 0;
    buf[n++] = prefix;

    int x = (prefix == 't' ? temp_counter++ : label_counter++);
    char numbuf[16];
    int len = itoa64(x, numbuf);
    for (int i = 0; i < len; i++) buf[n++] = numbuf[i];
    buf[n] = '\0';
    return buf;
}

static char *make_var(char *name) {
    char *buf = ast_arena_alloc(129);
    buf[0] = 'u';
    buf[1] = '_';
    size_t n = 0;
    while (n < 127 && name[n]) {
        buf[n + 2] = name[n];
        n++;
    }
    n++;
    n++;
    buf[n] = '\0';
    return buf;
}

static char *new_temp(void) { return make_name('t'); }
static char *new_label(void) { return make_name('L'); }

static IRInst *make_inst(void) { return ast_arena_alloc(sizeof(IRInst)); }

static void ir_emit(IRFunction *fn, IRInst *inst) {
    if (fn->n_code >= IR_MAX_CODE) {
        write(STDOUT_FILENO, "IR overflow\n", 11);
        return;
    }
    fn->code[fn->n_code++] = inst;
}

static char *lower_expr(ASTNode *expr, IRFunction *fn) {
    switch (expr->kind) {
        case AST_INT_LITERAL: {
            // tX = const VALUE
            IRInst *inst = make_inst();
            inst->op = IR_CONST;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }

            char *litbuf = ast_arena_alloc(32);
            int len;
            if (expr->data.int_lit.u_val == 0 && expr->data.int_lit.i_val < 0) {
                // negative
                len = itoa64(expr->data.int_lit.i_val, litbuf);
            } else if (expr->data.int_lit.u_val == 0 &&
                       expr->data.int_lit.i_val >= 0) {
                // non-negative signed
                len = itoa64(expr->data.int_lit.i_val, litbuf);
            } else {
                // unsigned
                len = utoa64(expr->data.int_lit.u_val, litbuf);
            }

            litbuf[len] = '\0';

            inst->arg1 = litbuf;

            Type *type = get_type(expr->data.int_lit.type->data.text.name);
            inst->type = type;

            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_IDENTIFIER: {
            return make_var(expr->data.text.name);
        }

        case AST_ADD_EXPR: {
            ASTNode *L = expr->first_child;
            ASTNode *R = L->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_ADD;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(L, fn);
            char *rt = lower_expr(R, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_SUB_EXPR: {
            ASTNode *L = expr->first_child;
            ASTNode *R = L->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_SUB;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(L, fn);
            char *rt = lower_expr(R, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_MUL_EXPR: {
            ASTNode *L = expr->first_child;
            ASTNode *R = L->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_MUL;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(L, fn);
            char *rt = lower_expr(R, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_DIV_EXPR: {
            ASTNode *L = expr->first_child;
            ASTNode *R = L->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_DIV;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(L, fn);
            char *rt = lower_expr(R, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_MOD_EXPR: {
            ASTNode *L = expr->first_child;
            ASTNode *R = L->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_REM;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(L, fn);
            char *rt = lower_expr(R, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_BW_NOT_EXPR: {
            ASTNode *rhs = expr->first_child;

            IRInst *inst = make_inst();
            inst->op = IR_BW_NOT_EXPR;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *rt = lower_expr(rhs, fn);
            inst->arg1 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_SHIFT_RIGHT: {
            ASTNode *lhs = expr->first_child;
            ASTNode *rhs = lhs->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_SHIFT_RIGHT;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(lhs, fn);
            char *rt = lower_expr(rhs, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_SHIFT_LEFT: {
            ASTNode *lhs = expr->first_child;
            ASTNode *rhs = lhs->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_SHIFT_LEFT;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(lhs, fn);
            char *rt = lower_expr(rhs, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_BW_AND_EXPR: {
            ASTNode *lhs = expr->first_child;
            ASTNode *rhs = lhs->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_BW_AND_EXPR;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(lhs, fn);
            char *rt = lower_expr(rhs, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_BW_XOR_EXPR: {
            ASTNode *lhs = expr->first_child;
            ASTNode *rhs = lhs->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_BW_XOR_EXPR;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(lhs, fn);
            char *rt = lower_expr(rhs, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_BW_OR_EXPR: {
            ASTNode *lhs = expr->first_child;
            ASTNode *rhs = lhs->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_BW_OR_EXPR;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }
            char *lt = lower_expr(lhs, fn);
            char *rt = lower_expr(rhs, fn);
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_CALL_EXPR: {
            ASTNode *id = expr->first_child;
            ASTNode *arglist = id->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_CALL;
            if (name_to_assign) {
                inst->dst = make_var(name_to_assign->data.text.name);
                name_to_assign = NULL;
            } else {
                char *temp = new_temp();
                inst->dst = temp;
            }

            int count = 0;
            for (ASTNode *a = arglist->first_child; a; a = a->next_sibling)
                count++;
            if (count > IR_MAX_CALL_ARGS) count = IR_MAX_CALL_ARGS;

            char *args[IR_MAX_CALL_ARGS];
            int idx = 0;
            for (ASTNode *a = arglist->first_child; a && idx < count;
                 a = a->next_sibling, idx++) {
                args[idx] = lower_expr(a, fn);
            }

            inst->arg1 = id->data.text.name;
            inst->nargs = idx;
            for (int j = 0; j < idx; j++) inst->args[j] = args[j];

            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;

            ir_emit(fn, inst);
            return inst->dst;
        }

        case AST_EXPRESSION:
            return lower_expr(expr->first_child, fn);

        default:
            return NULL;
    }
}

static void lower_stmt(ASTNode *stmt, IRFunction *fn) {
    switch (stmt->kind) {
        case AST_CONST_DECL:
        case AST_MUT_DECL: {
            ASTNode *name = stmt->first_child->next_sibling;
            ASTNode *init = name->next_sibling;

            IRInst *inst = make_inst();
            inst->op = IR_STORE;
            if (init) {
                name_to_assign = name;
                char *val = lower_expr(init, fn);
                if (val[0] == 'u') {
                    break;
                }
                name_to_assign = NULL;
                inst->arg1 = val;
            } else {
                inst->arg1 = "NULL";
            }
            inst->dst = make_var(name->data.text.name);
            inst->type =
                get_type(symtab_find_var(name->data.text.name)->data.text.name);
            ir_emit(fn, inst);
            break;
        }

        case AST_ASSIGN_STMT: {
            ASTNode *lhs = stmt->first_child;
            ASTNode *rhs = lhs->next_sibling;

            name_to_assign = lhs;
            char *val = lower_expr(rhs, fn);
            if (val[0] != 't') {
                break;
            }
            name_to_assign = NULL;
            IRInst *inst = make_inst();
            inst->op = IR_STORE;
            inst->arg1 = val;
            inst->dst = make_var(lhs->data.text.name);
            inst->type =
                get_type(symtab_find_var(lhs->data.text.name)->data.text.name);
            ir_emit(fn, inst);
            break;
        }

        case AST_RETURN_STMT: {
            char *val = lower_expr(stmt->first_child, fn);
            IRInst *inst = make_inst();
            inst->op = IR_RET;
            inst->arg1 = val;
            inst->type = get_type(stmt->type->data.text.name);
            ir_emit(fn, inst);
            break;
        }

        case AST_EXPR_STMT: {
            lower_expr(stmt->first_child, fn);
            break;
        }

        case AST_BLOCK:
            for (ASTNode *c = stmt->first_child; c; c = c->next_sibling)
                lower_stmt(c, fn);
            break;

        default:
            break;
    }
}

void irgen_program(ASTNode *ast_root, IRProgram *pr) {
    init_builtin_types();
    pr->n_funcs = 0;
    for (ASTNode *fn = ast_root->first_child; fn; fn = fn->next_sibling) {
        IRFunction *F = ast_arena_alloc(sizeof *F);
        if (!F) {
            write(STDOUT_FILENO, "OOM IR FUNCTION\n", 16);
            return;
        }

        pr->funcs[pr->n_funcs++] = F;
        F->name = fn->first_child->next_sibling->data.text.name;
        F->n_params = 0;
        // Collect n_params
        ASTNode *plist = fn->first_child->next_sibling->next_sibling;
        for (ASTNode *p = plist->first_child; p; p = p->next_sibling) {
            F->params[F->n_params++] =
                make_var(p->first_child->next_sibling->data.text.name);
        }
        F->n_code = 0;

        ASTNode *body = plist->next_sibling;
        lower_stmt(body, F);
    }
}
