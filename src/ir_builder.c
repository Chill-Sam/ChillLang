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

void ir_builder_init(IrBuilder *b, IrFunc *fn) {
    b->func          = fn;
    b->next_label_id = 0;

    b->block         = ir_func_new_block(fn);
    fn->entry        = b->block;
    fn->blocks       = b->block;
}

IrValue irb_new_value(IrBuilder *b, TypeId type) {
    return ir_func_new_value(b->func, type);
}

void irb_emit(IrBuilder *b, IrInst inst) {
    IrInst *new_inst = malloc(sizeof(IrInst));
    *new_inst        = inst;
    new_inst->next   = NULL;
    new_inst->prev   = NULL;
    new_inst->block  = b->block;

    if (!b->block->first) {
        b->block->first = new_inst;
        b->block->last  = new_inst;
    } else {
        new_inst->prev       = b->block->last;
        b->block->last->next = new_inst;
        b->block->last       = new_inst;
    }
    b->block->last = new_inst;
}

IrValue irb_const_int(IrBuilder *b, TypeId type, int64_t imm) {
    IrValue dst = irb_new_value(b, type);

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
    IrValue dst = irb_new_value(b, type);

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
    IrValue dst = irb_new_value(b, type);

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

IrValue irb_mov(IrBuilder *b, TypeId type, IrValue src, IrValue dst) {
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

IrValue irb_unary_cast(IrBuilder *b, IrOp op, TypeId dst_type, IrValue src) {
    IrValue dst = irb_new_value(b, dst_type);

    IrInst inst;
    inst.op   = op;
    inst.type = dst_type;
    inst.dst  = dst;
    inst.src0 = src;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    irb_emit(b, inst);
    return dst;
}

void irb_store(IrBuilder *b, IrValue addr, IrValue src) {
    IrInst inst;
    inst.op   = IR_OP_STORE;
    inst.type = TYPEID_VOID;
    inst.dst  = addr;
    inst.src0 = src;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    irb_emit(b, inst);
}

IrValue irb_load(IrBuilder *b, TypeId type, IrValue addr) {
    IrValue dst = irb_new_value(b, type);

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

void irb_ret_void(IrBuilder *b) {
    IrInst inst;
    inst.op   = IR_OP_RET;
    inst.type = TYPEID_VOID;
    inst.dst  = (IrValue)~0u;
    inst.src0 = (IrValue)~0u;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    irb_emit(b, inst);
}

void irb_ret(IrBuilder *b, IrValue value, TypeId type) {
    IrInst inst;
    inst.op   = IR_OP_RET;
    inst.type = type;
    inst.dst  = (IrValue)~0u;
    inst.src0 = value;
    inst.src1 = (IrValue)~0u;
    inst.imm  = 0;

    irb_emit(b, inst);
}

IrValue irb_call(IrBuilder *b, TypeId ret_type, const char *name,
                 uint32_t num_args, const IrValue *args) {
    if (num_args > 6) {
        fprintf(stderr,
                "lowering error: more than 6 call args not supported\n");
        abort();
    }

    IrValue dst =
        (ret_type == TYPEID_VOID) ? (IrValue)~0u : irb_new_value(b, ret_type);

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

IrValue irb_phi(IrBuilder *b, TypeId type, uint32_t count, IrValue *values,
                int *labels) {
    IrValue dst = irb_new_value(b, type);

    IrInst inst;
    inst.op         = IR_OP_PHI;
    inst.type       = type;
    inst.dst        = dst;
    inst.src0       = (IrValue)~0u;
    inst.src1       = (IrValue)~0u;
    inst.imm        = 0;

    inst.phi.count  = count;
    inst.phi.values = malloc(count * sizeof(IrValue));
    inst.phi.labels = malloc(count * sizeof(int));

    for (uint32_t i = 0; i < count; i++) {
        inst.phi.values[i] = values[i];
        inst.phi.labels[i] = labels[i];
    }

    irb_emit(b, inst);
    return dst;
}

int irb_new_label(IrBuilder *b) { return b->next_label_id++; }

void irb_mark_label(IrBuilder *b, int label_id) {
    IrBlock *new_block     = ir_func_new_block(b->func);
    new_block->label_value = label_id;
    b->block               = new_block;

    IrInst inst;
    inst.op   = IR_OP_LABEL;
    inst.type = TYPEID_INVALID;
    inst.dst  = (IrValue)~0u;
    inst.src0 = (IrValue)~0u;
    inst.src1 = (IrValue)~0u;
    inst.imm  = label_id;

    irb_emit(b, inst);
}

void irb_br(IrBuilder *b, int label_id) {
    IrInst inst;
    inst.op   = IR_OP_BR;
    inst.type = TYPEID_INVALID;
    inst.dst  = (IrValue)~0u;
    inst.src0 = (IrValue)~0u;
    inst.src1 = (IrValue)~0u;
    inst.imm  = label_id;

    irb_emit(b, inst);
}

void irb_brcond(IrBuilder *b, IrValue cond, int label_true) {
    IrInst inst;
    inst.op   = IR_OP_BRCOND;
    inst.type = TYPEID_INVALID;
    inst.dst  = (IrValue)~0u;
    inst.src0 = cond;
    inst.src1 = (IrValue)~0u;
    inst.imm  = label_true;

    irb_emit(b, inst);

    IrBlock *new_block = ir_func_new_block(b->func);
    b->block           = new_block;
}
