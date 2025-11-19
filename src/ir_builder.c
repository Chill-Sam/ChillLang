#include "ir_builder.h"
#include <stdlib.h>
#include <string.h>

static char *irb_strdup(const char *s) {
    size_t len = strlen(s);
    char *copy = malloc(len + 1);
    if (!copy) {
        fprintf(stderr, "fatal: out of memory in irb_strdup\n");
        abort();
    }
    memcpy(copy, s, len + 1);
    return copy;
}

void ir_builder_init(IrBuilder *b, IrFunc *fn) { b->func = fn; }

IrValue irb_new_value(IrBuilder *b) { return ir_func_new_value(b->func); }
IrInstId irb_emit(IrBuilder *b, IrInst inst) {
    return ir_func_add_inst(b->func, inst);
}

IrValue irb_const_int(IrBuilder *b, TypeId type, int64_t imm) {
    IrValue dst = irb_new_value(b);

    IrInst inst;
    inst.op   = IR_CONST_INT;
    inst.type = type;
    inst.dst  = dst;
    inst.src0 = (IrValue)~0u;
    inst.src1 = (IrValue)~0u;
    inst.imm  = imm;

    irb_emit(b, inst);
    return dst;
}

IrValue irb_binop(IrBuilder *b, IrOp op, TypeId type, IrValue lhs,
                  IrValue rhs) {
    IrValue dst = irb_new_value(b);

    IrInst inst;
    inst.op   = op;
    inst.type = type;
    inst.dst  = dst;
    inst.src0 = lhs;
    inst.src1 = rhs;
    inst.imm  = 0;

    irb_emit(b, inst);
    return dst;
}

IrValue irb_unop(IrBuilder *b, IrOp op, TypeId type, IrValue src) {
    IrValue dst = irb_new_value(b);

    IrInst inst;
    inst.op   = op;
    inst.type = type;
    inst.dst  = dst;
    inst.src0 = src;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    irb_emit(b, inst);
    return dst;
}

IrValue irb_mov(IrBuilder *b, TypeId type, IrValue src) {
    IrValue dst = irb_new_value(b);

    IrInst inst;
    inst.op   = IR_OP_MOV;
    inst.type = type;
    inst.dst  = dst;
    inst.src0 = src;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    irb_emit(b, inst);
    return dst;
}

IrInstId irb_store(IrBuilder *b, IrValue addr, IrValue src) {
    IrInst inst;
    inst.op   = IR_OP_STORE;
    inst.type = TYPEID_VOID;
    inst.dst  = addr;
    inst.src0 = src;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    return irb_emit(b, inst);
}

IrValue irb_load(IrBuilder *b, TypeId type, IrValue addr) {
    IrValue dst = irb_new_value(b);

    IrInst inst;
    inst.op   = IR_OP_LOAD;
    inst.type = type;
    inst.dst  = dst;
    inst.src0 = addr;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    irb_emit(b, inst);
    return dst;
}

IrInstId irb_ret_void(IrBuilder *b) {
    IrInst inst;
    inst.op   = IR_OP_RET;
    inst.type = TYPEID_VOID;
    inst.dst  = (IrValue)~0u;
    inst.src0 = (IrValue)~0u;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    return irb_emit(b, inst);
}

IrInstId irb_ret(IrBuilder *b, IrValue value) {
    IrInst inst;
    inst.op   = IR_OP_RET;
    inst.type = TYPEID_VOID;
    inst.dst  = (IrValue)~0u;
    inst.src0 = value;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    return irb_emit(b, inst);
}

IrValue irb_call(IrBuilder *b, TypeId ret_type, const char *name,
                 uint32_t num_args, const IrValue *args) {
    if (num_args > 6) {
        fprintf(stderr,
                "lowering error: more than 6 call args not supported\n");
        abort();
    }

    IrValue dst = (ret_type == TYPEID_VOID) ? (IrValue)~0u : irb_new_value(b);

    IrInst inst;
    inst.op             = IR_OP_CALL;
    inst.type           = ret_type;
    inst.dst            = dst;
    inst.src0           = (IrValue)~0u;
    inst.src1           = (IrValue)~0u;
    inst.imm            = 0;

    inst.call_name      = irb_strdup(name);
    inst.call_arg_count = (uint8_t)num_args;
    for (uint32_t i = 0; i < num_args; i++) {
        inst.call_args[i] = args[i];
    }

    irb_emit(b, inst);
    return dst;
}
