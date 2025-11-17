#pragma once

#include "ir.h"

typedef struct IrBuilder {
    IrFunc *func;
} IrBuilder;

void ir_builder_init(IrBuilder *b, IrFunc *fn);

IrValue irb_new_value(IrBuilder *b);
IrInstId irb_emit(IrBuilder *b, IrInst inst);

IrValue irb_const_int(IrBuilder *b, TypeId type, int64_t imm);
IrValue irb_binop(IrBuilder *b, IrOp op, TypeId type, IrValue lhs, IrValue rhs);
IrValue irb_unop(IrBuilder *b, IrOp op, TypeId type, IrValue src);
IrValue irb_mov(IrBuilder *b, TypeId type, IrValue src);

IrInstId irb_store(IrBuilder *b, IrValue addr, IrValue src);
IrValue irb_load(IrBuilder *b, TypeId type, IrValue addr);

IrInstId irb_ret_void(IrBuilder *b);
IrInstId irb_ret(IrBuilder *b, IrValue value);

IrValue irb_call(IrBuilder *b, TypeId ret_type, AstNode *func,
                 uint32_t num_args, const IrValue *args);
