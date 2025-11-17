#include "ir_builder.h"

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
    inst.func = NULL;

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
    inst.func = NULL;

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
    inst.func = NULL;

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
    inst.func = NULL;

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
    inst.func = NULL;

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
    inst.func = NULL;

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
    inst.func = NULL;

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
    inst.func = NULL;

    return irb_emit(b, inst);
}

IrValue irb_call(IrBuilder *b, TypeId ret_type, AstNode *func,
                 uint32_t num_args, const IrValue *args) {
    (void)num_args;
    (void)args;

    IrValue dst = (ret_type == TYPEID_VOID) ? (IrValue)~0u : irb_new_value(b);

    IrInst inst;
    inst.op   = IR_OP_CALL;
    inst.type = ret_type;
    inst.dst  = dst;
    inst.src0 = (IrValue)~0u;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;
    inst.func = func;

    irb_emit(b, inst);
    return dst;
}
