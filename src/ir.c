#include "ir.h"
#include <stdlib.h>

static void *xrealloc(void *ptr, size_t new_size, const char *where) {
    void *p = realloc(ptr, new_size);
    if (!p) {
        fprintf(stderr, "fatal: out of memory in %s\n", where);
        abort();
    }
    return p;
}

void ir_module_init(IrModule *m) {
    m->funcs       = NULL;
    m->funcs_count = 0;
    m->funcs_cap   = 0;
}

void free_ir_module(IrModule *m) {
    if (!m)
        return;
    for (uint32_t i = 0; i < m->funcs_count; i++) {
        ir_func_free(&m->funcs[i]);
    }
    free(m->funcs);
    free(m);
}

IrFunc ir_func_create(char *name, TypeId ret_type, uint32_t num_params) {
    IrFunc fn;
    fn.name        = name;
    fn.return_type = ret_type;
    fn.num_args    = num_params;

    fn.entry       = NULL;
    fn.blocks      = NULL;
    fn.block_count = 0;

    fn.value_count = num_params;
    fn.value_types = NULL;
    if (num_params > 0) {
        fn.value_types = malloc(num_params * sizeof(TypeId));
        if (!fn.value_types) {
            fprintf(stderr, "fatal: out of memory in ir_func_create\n");
            abort();
        }
    }

    return fn;
}

void ir_func_free(IrFunc *fn) {
    if (!fn)
        return;
    free(fn->name);
    free(fn->value_types);
    IrBlock *b = fn->entry;
    while (b) {
        IrBlock *next = b->next;

        IrInst *inst  = b->first;
        while (inst) {
            IrInst *next_inst = inst->next;

            if (inst->op == IR_OP_CALL && inst->call_name) {
                free(inst->call_name);
            }

            if (inst->op == IR_OP_PHI) {
                free(inst->phi.values);
                free(inst->phi.labels);
            }

            free(inst);
            inst = next_inst;
        }

        free(b->preds);
        free(b->succs);
        free(b->dom_children);
        free(b->dom_frontier);
        free(b);

        b = next;
    }
}

void ir_module_add_func(IrModule *m, IrFunc fn) {
    if (m->funcs_count == m->funcs_cap) {
        uint32_t new_cap = m->funcs_cap ? m->funcs_cap * 2 : 4;
        m->funcs =
            xrealloc(m->funcs, new_cap * sizeof(IrFunc), "ir_module_add_func");
        m->funcs_cap = new_cap;
    }
    m->funcs[m->funcs_count++] = fn;
}

IrValue ir_func_new_value(IrFunc *fn, TypeId type) {
    IrValue v = fn->value_count;
    fn->value_count++;

    fn->value_types = xrealloc(
        fn->value_types, fn->value_count * sizeof(TypeId), "ir_func_new_value");
    fn->value_types[v] = type;

    return (IrValue)v;
}

void add_block_to_func(IrFunc *fn, IrBlock *block) {
    if (!fn->blocks) {
        fn->blocks = block;
    } else {
        IrBlock *last = fn->blocks;
        while (last->next)
            last = last->next;
        last->next = block;
    }
}

IrBlock *ir_func_new_block(IrFunc *fn) {
    IrBlock *block     = calloc(1, sizeof(IrBlock));
    block->id          = fn->block_count++;
    block->label_value = -1;
    block->first       = NULL;
    block->last        = NULL;

    // CFG edges
    block->preds      = NULL;
    block->succs      = NULL;
    block->pred_count = 0;
    block->succ_count = 0;
    block->next       = NULL;

    // Dominance info
    block->rpo_index          = -1;
    block->idom               = NULL;
    block->dom_children       = NULL;
    block->dom_children_count = 0;
    block->dom_children_cap   = 0;
    block->dom_frontier       = NULL;
    block->dom_frontier_count = 0;
    block->dom_frontier_cap   = 0;

    add_block_to_func(fn, block);
    return block;
}

IrBlock *find_block_by_label(IrFunc *fn, int label) {
    for (IrBlock *b = fn->entry; b; b = b->next) {
        if (b->label_value == label)
            return b;
    }
    return NULL;
}

// ------ Dumping ------
static const char *ir_op_name(IrOp op) {
    switch (op) {
    case IR_OP_INVALID:
        return "INVALID";
    case IR_CONST_INT:
        return "CONST_INT";

    case IR_OP_ADD:
        return "ADD";
    case IR_OP_SUB:
        return "SUB";
    case IR_OP_MUL:
        return "MUL";
    case IR_OP_DIV:
        return "DIV";
    case IR_OP_MOD:
        return "MOD";

    case IR_OP_AND:
        return "AND";
    case IR_OP_OR:
        return "OR";
    case IR_OP_XOR:
        return "XOR";
    case IR_OP_SHL:
        return "SHL";
    case IR_OP_SHR:
        return "SHR";

    case IR_OP_CMP_LT:
        return "CMP_LT";
    case IR_OP_CMP_LE:
        return "CMP_LE";
    case IR_OP_CMP_GT:
        return "CMP_GT";
    case IR_OP_CMP_GE:
        return "CMP_GE";
    case IR_OP_CMP_EQ:
        return "CMP_EQ";
    case IR_OP_CMP_NE:
        return "CMP_NE";

    case IR_OP_NEG:
        return "NEG";
    case IR_OP_NOT:
        return "NOT";
    case IR_OP_BITNOT:
        return "BITNOT";

    case IR_OP_LOGICAL_AND:
        return "LOGICAL_AND";
    case IR_OP_LOGICAL_OR:
        return "LOGICAL_OR";

    case IR_OP_ZEXT:
        return "ZEXT";
    case IR_OP_SEXT:
        return "SEXT";
    case IR_OP_TRUNC:
        return "TRUNC";

    case IR_OP_MOV:
        return "MOV";

    case IR_OP_ALLOCA:
        return "ALLOCA";
    case IR_OP_LOAD:
        return "LOAD";
    case IR_OP_STORE:
        return "STORE";

    case IR_OP_LABEL:
        return "LABEL";
    case IR_OP_BR:
        return "BR";
    case IR_OP_BRCOND:
        return "BRCOND";
    case IR_OP_PHI:
        return "PHI";

    case IR_OP_CALL:
        return "CALL";
    case IR_OP_RET:
        return "RET";
    }
    return "UNKNOWN";
}

static void ir_dump_instr(const IrInst *in, FILE *out) {
    fprintf(out, "  %s", ir_op_name(in->op));

    if (in->dst != (IrValue)~0u) {
        fprintf(out, " v%u =", in->dst);
    }

    switch (in->op) {
    case IR_CONST_INT:
        fprintf(out, " %lld", (long long)in->imm);
        break;

    case IR_OP_ADD:
    case IR_OP_SUB:
    case IR_OP_MUL:
    case IR_OP_DIV:
    case IR_OP_MOD:
    case IR_OP_AND:
    case IR_OP_OR:
    case IR_OP_XOR:
    case IR_OP_SHL:
    case IR_OP_SHR:
    case IR_OP_CMP_LT:
    case IR_OP_CMP_LE:
    case IR_OP_CMP_GT:
    case IR_OP_CMP_GE:
    case IR_OP_CMP_EQ:
    case IR_OP_CMP_NE:
        fprintf(out, " v%u, v%u", in->src0, in->src1);
        break;

    case IR_OP_NEG:
    case IR_OP_NOT:
    case IR_OP_BITNOT:
    case IR_OP_MOV:
        fprintf(out, " v%u", in->src0);
        break;

    case IR_OP_LOGICAL_AND:
    case IR_OP_LOGICAL_OR:
        fprintf(out, " v%u, v%u", in->src0, in->src1);
        break;

    case IR_OP_ZEXT:
    case IR_OP_SEXT:
    case IR_OP_TRUNC:
        fprintf(out, " v%u as %d", in->src0, in->type);
        break;

    case IR_OP_ALLOCA:
        fprintf(out, " as %d", in->type);
        break;

    case IR_OP_LOAD:
        fprintf(out, " [v%u]", in->src0);
        break;

    case IR_OP_STORE:
        fprintf(out, " v%u", in->src0);
        break;

    case IR_OP_LABEL:
    case IR_OP_BR:
        fprintf(out, " %ld", in->imm);
        break;

    case IR_OP_BRCOND:
        fprintf(out, " v%u, %ld", in->src0, in->imm);
        break;

    case IR_OP_PHI:
        for (uint8_t i = 0; i < in->phi.count; i++) {
            if (i > 0)
                fprintf(out, ",");
            fprintf(out, " [v%u, L%d]", in->phi.values[i], in->phi.labels[i]);
        }
        break;

    case IR_OP_CALL:
        fprintf(out, " %s(", in->call_name ? in->call_name : "<null>");
        for (uint8_t i = 0; i < in->call_arg_count; i++) {
            if (i)
                fputs(", ", out);
            fprintf(out, "v%u", in->call_args[i]);
        }
        fputs(")", out);
        break;

    case IR_OP_RET:
        if (in->src0 != (IrValue)~0u) {
            fprintf(out, " v%u", in->src0);
        }
        break;

    default:
        break;
    }

    if (in->type != TYPEID_INVALID) {
        fprintf(out, " type: %d ", in->type);
    }

    fputc('\n', out);
}

void ir_dump_func(const IrFunc *fn, FILE *out) {
    fprintf(out, "func %s(", fn->name ? fn->name : "<anon>");
    fprintf(out, "params=%u", fn->num_args);
    fprintf(out, ") ret_type=%u\n", (unsigned)fn->return_type);

    uint32_t j     = 0;
    IrBlock *block = fn->entry;
    for (uint32_t i = 0; i < fn->block_count; i++) {
        if (!block->first) {
            continue;
        }
        fprintf(out, "BLOCK %u:", block->id);
        if (block->label_value != -1) {
            fprintf(out, " L%d,", block->label_value);
        }

        if (block->pred_count > 0) {
            fprintf(out, " preds=");
            for (uint32_t j = 0; j < block->pred_count; j++) {
                if (j > 0)
                    fprintf(out, ",");
                fprintf(out, "%u", block->preds[j]->id);
            }
        }

        if (block->succ_count > 0) {
            fprintf(out, " succs=");
            for (uint32_t j = 0; j < block->succ_count; j++) {
                if (j > 0)
                    fprintf(out, ",");
                fprintf(out, "%u", block->succs[j]->id);
            }
        }

        if (block->rpo_index != -1) {
            fprintf(out, " rpo=%d", block->rpo_index);
        }

        if (block->idom) {
            fprintf(out, " idom=%u", block->idom->id);
        }

        if (block->dom_children_count > 0) {
            fprintf(out, " dom=");
            for (uint32_t j = 0; j < block->dom_children_count; j++) {
                if (j > 0)
                    fprintf(out, ",");
                fprintf(out, "%u", block->dom_children[j]->id);
            }
        }

        if (block->dom_frontier_count > 0) {
            fprintf(out, " frontier=");
            for (uint32_t j = 0; j < block->dom_frontier_count; j++) {
                if (j > 0)
                    fprintf(out, ",");
                fprintf(out, "%u", block->dom_frontier[j]->id);
            }
        }

        fputc('\n', out);

        IrInst *cur_inst = block->first;
        while (cur_inst != block->last) {
            fprintf(out, "%4u:", j);
            ir_dump_instr(cur_inst, out);
            cur_inst = cur_inst->next;
            j++;
        }
        fprintf(out, "%4u:", j);
        ir_dump_instr(cur_inst, out);
        j++;

        block = block->next;
    }
    fputs("\n", out);
}

void ir_dump_module(const IrModule *m, FILE *out) {
    fprintf(out, "IRModule: %u functions\n", m->funcs_count);
    for (uint32_t i = 0; i < m->funcs_count; i++) {
        ir_dump_func(&m->funcs[i], out);
    }
}
