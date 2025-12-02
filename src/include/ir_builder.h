#pragma once

#include "ir.h"

typedef struct IrBuilder {
    IrFunc *func;
    IrBlock *block;
    int next_label_id;

    // Break and continue labels
    int start_label_id;
    int end_label_id;
} IrBuilder;

void ir_builder_init(IrBuilder *b, IrFunc *fn);

IrValue irb_new_value(IrBuilder *b, TypeId type);
void irb_emit(IrBuilder *b, IrInst inst);

IrValue irb_const_int(IrBuilder *b, TypeId type, int64_t imm);
IrValue irb_binop(IrBuilder *b, IrOp op, TypeId type, IrValue lhs, IrValue rhs);
IrValue irb_unop(IrBuilder *b, IrOp op, TypeId type, IrValue src);
IrValue irb_unary_cast(IrBuilder *b, IrOp op, TypeId dst_type, IrValue src);
IrValue irb_mov(IrBuilder *b, TypeId type, IrValue src, IrValue dst);

IrValue irb_alloca(IrBuilder *b, TypeId type);
void irb_store(IrBuilder *b, IrValue addr, IrValue src);
IrValue irb_load(IrBuilder *b, TypeId type, IrValue addr);

void irb_ret_void(IrBuilder *b);
void irb_ret(IrBuilder *b, IrValue value, TypeId type);

IrValue irb_call(IrBuilder *b, TypeId ret_type, const char *name,
                 uint32_t num_args, const IrValue *args);
IrValue irb_phi(IrBuilder *b, TypeId type, uint32_t count, IrValue *values,
                int *labels);

int irb_new_label(IrBuilder *b);
void irb_mark_label(IrBuilder *b, int label_id);
void irb_br(IrBuilder *b, int label_id);
void irb_brcond(IrBuilder *b, IrValue cond, int label_true);
