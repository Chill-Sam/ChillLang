#pragma once

#include "ast.h"
#include "type.h"
#include <stdint.h>
#include <stdio.h>

typedef uint32_t IrValue;
typedef uint32_t IrInstId;

typedef enum IrOp {
    IR_OP_INVALID = 0,

    IR_CONST_INT, // dst = imm

    IR_OP_ADD,
    IR_OP_SUB,
    IR_OP_MUL,
    IR_OP_DIV, // TODO: allow float div
    IR_OP_MOD,

    IR_OP_AND,
    IR_OP_OR,
    IR_OP_XOR,
    IR_OP_SHL,
    IR_OP_SHR,

    IR_OP_CMP_LT,
    IR_OP_CMP_LE,
    IR_OP_CMP_GT,
    IR_OP_CMP_GE,
    IR_OP_CMP_EQ,
    IR_OP_CMP_NE,

    IR_OP_NEG,
    IR_OP_NOT,
    IR_OP_BITNOT,

    IR_OP_MOV, // dst = src0

    IR_OP_LOAD,  // dst = [addr]
    IR_OP_STORE, //[addr] = src0

    IR_OP_CALL, // dst = call func
    IR_OP_RET,  // ret src0
} IrOp;

typedef struct IrInst {
    IrOp op;
    TypeId type; // result type
    IrValue dst; // virtual register
    IrValue src0;
    IrValue src1;
    int64_t imm;   // NOTE: CONST_INT only
    AstNode *func; // pointer to AST_FUNC_DECL (NOTE: for now)
} IrInst;

typedef struct IrFunc {
    char *name;
    TypeId return_type;
    uint32_t num_args;

    IrInst *insts;
    uint32_t insts_count;
    uint32_t insts_cap;

    uint32_t value_count; // number of virtual values used
} IrFunc;

typedef struct IrModule {
    IrFunc *funcs;
    uint32_t funcs_count;
    uint32_t funcs_cap;
} IrModule;

// Module / function lifetime
void ir_module_init(IrModule *m);
void free_ir_module(IrModule *m);

IrFunc ir_func_create(char *name, TypeId ret_type, uint32_t num_params);
void ir_func_free(IrFunc *fn);

void ir_module_add_func(IrModule *m, IrFunc fn);

// Instruction emission
IrInstId ir_func_add_inst(IrFunc *fn, IrInst inst);

// Virtual value management
IrValue ir_func_new_value(IrFunc *fn);

// Debug dump
void ir_dump_func(const IrFunc *fn, FILE *out);
void ir_dump_module(const IrModule *m, FILE *out);
