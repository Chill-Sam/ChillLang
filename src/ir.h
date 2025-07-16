#pragma once

#include "int.c"
#include "ir_type.h"

#define IR_MAX_FUNCS 128
#define IR_MAX_CODE 1024
#define IR_MAX_CALL_ARGS 16

typedef enum {
    IR_CONST,
    IR_LOAD,
    IR_STORE,
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_REM,
    IR_CALL,
    IR_RET,
    IR_LABEL,
    IR_BR,
} IrOp;

typedef struct IRInst {
    IrOp op;
    char *dst;
    char *arg1;
    char *arg2;
    char *arg3;

    // For IR_CALL we need variable-length list of args
    char *args[IR_MAX_CALL_ARGS];
    size_t nargs;

    Type *type;
} IRInst;

typedef struct IRFunction {
    char *name;
    char *params[IR_MAX_CALL_ARGS];
    size_t n_params;

    IRInst *code[IR_MAX_CODE];
    size_t n_code;
} IRFunction;

typedef struct IRProgram {
    IRFunction *funcs[IR_MAX_FUNCS];
    size_t n_funcs;
} IRProgram;
