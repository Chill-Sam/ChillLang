#include "irgen.h"

#include "file_stream.c"
#include "ir.h"
#include "ir_type.c"
#include "symtab.h"

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
            char *temp = new_temp();
            IRInst *inst = make_inst();
            inst->op = IR_CONST;
            inst->dst = temp;

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
            return temp;
        }

        case AST_IDENTIFIER: {
            // tX = load var
            char *temp = new_temp();
            IRInst *inst = make_inst();
            inst->op = IR_LOAD;
            inst->dst = temp;
            inst->arg1 = expr->data.text.name;
            Type *type =
                get_type(symtab_find_var(expr->data.text.name)->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return temp;
        }

        case AST_ADD_EXPR: {
            ASTNode *L = expr->first_child;
            ASTNode *R = L->next_sibling;

            char *lt = lower_expr(L, fn);
            char *rt = lower_expr(R, fn);
            char *temp = new_temp();
            IRInst *inst = make_inst();
            inst->op = IR_ADD;
            inst->dst = temp;
            inst->arg1 = lt;
            inst->arg2 = rt;
            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;
            ir_emit(fn, inst);
            return temp;
        }

        case AST_CALL_EXPR: {
            ASTNode *id = expr->first_child;
            ASTNode *arglist = id->next_sibling;

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

            char *temp = new_temp();
            IRInst *inst = make_inst();
            inst->op = IR_CALL;
            inst->dst = temp;
            inst->arg1 = id->data.text.name;
            inst->nargs = idx;
            for (int j = 0; j < idx; j++) inst->args[j] = args[j];

            Type *type = get_type(expr->type->data.text.name);
            inst->type = type;

            ir_emit(fn, inst);
            return temp;
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
                char *val = lower_expr(init, fn);
                inst->arg1 = val;
            } else {
                inst->arg1 = "NULL";
            }
            inst->arg2 = name->data.text.name;
            inst->type =
                get_type(symtab_find_var(name->data.text.name)->data.text.name);
            ir_emit(fn, inst);
            break;
        }

        case AST_ASSIGN_STMT: {
            ASTNode *lhs = stmt->first_child;
            ASTNode *rhs = lhs->next_sibling;

            char *val = lower_expr(rhs, fn);
            IRInst *inst = make_inst();
            inst->op = IR_STORE;
            inst->arg1 = val;
            inst->arg2 = lhs->data.text.name;
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
                p->first_child->next_sibling->data.text.name;
        }
        F->n_code = 0;

        ASTNode *body = plist->next_sibling;
        lower_stmt(body, F);
    }
}
