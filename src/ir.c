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
        free(m->funcs[i].insts);
        free(m->funcs[i].name);
    }
    free(m->funcs);
    free(m);
}

IrFunc ir_func_create(char *name, TypeId ret_type, uint32_t num_params) {
    IrFunc fn;
    fn.name        = name;
    fn.return_type = ret_type;
    fn.num_args    = num_params;

    fn.insts       = NULL;
    fn.insts_count = 0;
    fn.insts_cap   = 0;

    fn.value_count = num_params;

    return fn;
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

IrInstId ir_func_add_inst(IrFunc *fn, IrInst inst) {
    if (fn->insts_count == fn->insts_cap) {
        uint32_t new_cap = fn->insts_cap ? fn->insts_cap * 2 : 4;
        fn->insts =
            xrealloc(fn->insts, new_cap * sizeof(IrInst), "ir_func_add_inst");
        fn->insts_cap = new_cap;
    }
    fn->insts[fn->insts_count++] = inst;
    return fn->insts_count - 1;
}

IrValue ir_func_new_value(IrFunc *fn) {
    IrValue v = fn->value_count;
    fn->value_count++;
    return v;
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

    case IR_OP_MOV:
        return "MOV";

    case IR_OP_LOAD:
        return "LOAD";
    case IR_OP_STORE:
        return "STORE";

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

    case IR_OP_LOAD:
        fprintf(out, " [v%u]", in->src0);
        break;

    case IR_OP_STORE:
        fprintf(out, " [v%u] = v%u", in->dst, in->src0);
        break;

    case IR_OP_CALL:
        fprintf(out, " func=%p", (void *)in->func);
        break;

    case IR_OP_RET:
        if (in->src0 != (IrValue)~0u) {
            fprintf(out, " v%u", in->src0);
        }
        break;

    default:
        break;
    }

    fputc('\n', out);
}

void ir_dump_func(const IrFunc *fn, FILE *out) {
    fprintf(out, "func %s(", fn->name ? fn->name : "<anon>");
    fprintf(out, "params=%u", fn->num_args);
    fprintf(out, ") ret_type=%u\n", (unsigned)fn->return_type);

    for (uint32_t i = 0; i < fn->insts_count; i++) {
        fprintf(out, "%4u:", i);
        ir_dump_instr(&fn->insts[i], out);
    }
    fputs("\n", out);
}

void ir_dump_module(const IrModule *m, FILE *out) {
    fprintf(out, "IRModule: %u functions\n", m->funcs_count);
    for (uint32_t i = 0; i < m->funcs_count; i++) {
        ir_dump_func(&m->funcs[i], out);
    }
}
