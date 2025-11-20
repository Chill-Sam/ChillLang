#include "x64_codegen.h"
#include <stdio.h>
#include <stdlib.h>

static const char *ARG_REGS[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

static int stack_offset_for_value(const IrFunc *fn, IrValue v) {
    (void)fn;
    int idx = (int)v;
    return -8 * (idx + 1);
}

static int compute_frame_size(const IrFunc *fn) {
    int size = (int)fn->value_count * 8;

    // Keep stack 16 byte aligned at call sites
    if ((size % 16) == 0) {
        size += 8;
    }

    return size;
}

static void x64_emit_prologue(FILE *out, const IrFunc *fn, int frame_size) {
    (void)fn;

    fprintf(out, "    push rbp\n");
    fprintf(out, "    mov rbp, rsp\n");
    if (frame_size > 0) {
        fprintf(out, "    sub rsp, %d\n", frame_size);
    }

    fprintf(out, "\n");
}

static void x64_emit_epilogue(FILE *out) {
    fprintf(out, "    leave\n");
    fprintf(out, "    ret\n");
    fprintf(out, "\n");
}

static void x64_emit_inst(FILE *out, const IrFunc *fn, const IrInst *inst) {
    // TODO: Implement other instructions
    switch (inst->op) {
    case IR_CONST_INT: {
        int off = stack_offset_for_value(fn, inst->dst);
        fprintf(out, "    mov rax, %lld\n", (long long)inst->imm);
        fprintf(out, "    mov QWORD PTR [rbp%+d], rax\n", off);
        break;
    }

    case IR_OP_ADD:
    case IR_OP_SUB:
    case IR_OP_MUL:
    case IR_OP_DIV: {
        int off_dst = stack_offset_for_value(fn, inst->dst);
        int off0    = stack_offset_for_value(fn, inst->src0);
        int off1    = stack_offset_for_value(fn, inst->src1);

        fprintf(out, "    mov rax, QWORD PTR [rbp%+d]\n", off0);

        switch (inst->op) {
        case IR_OP_ADD:
            fprintf(out, "    add rax, QWORD PTR [rbp%+d]\n", off1);
            break;
        case IR_OP_SUB:
            fprintf(out, "    sub rax, QWORD PTR [rbp%+d]\n", off1);
            break;
        case IR_OP_MUL:
            fprintf(out, "    imul rax, QWORD PTR [rbp%+d]\n", off1);
            break;
        case IR_OP_DIV:
            fprintf(out, "    mov rbx, QWORD PTR [rbp%+d]\n", off1);
            fprintf(out, "    cqo\n");
            fprintf(out, "    idiv rbx\n");
            break;
        default:
            break;
        }

        fprintf(out, "    mov QWORD PTR [rbp%+d], rax\n", off_dst);
        break;
    }

    case IR_OP_MOV: {
        int off_dst = stack_offset_for_value(fn, inst->dst);
        int off_src = stack_offset_for_value(fn, inst->src0);

        fprintf(out, "    mov rax, QWORD PTR [rbp%+d]\n", off_src);
        fprintf(out, "    mov QWORD PTR [rbp%+d], rax\n", off_dst);
        break;
    }

    case IR_OP_RET: {
        if (inst->src0 != (IrValue)~0u) {
            int off = stack_offset_for_value(fn, inst->src0);
            fprintf(out, "    mov rax, QWORD PTR [rbp%+d]\n", off);
        }

        break;
    }

    case IR_OP_CALL: {
        for (uint8_t i = 0; i < inst->call_arg_count; i++) {
            IrValue arg_v   = inst->call_args[i];
            int off         = stack_offset_for_value(fn, arg_v);
            const char *reg = ARG_REGS[i];
            fprintf(out, "    mov %s, QWORD PTR [rbp%+d]\n", reg, off);
        }

        fprintf(out, "    call %s\n", inst->call_name);

        if (inst->dst != (IrValue)~0u) {
            int off_dst = stack_offset_for_value(fn, inst->dst);
            fprintf(out, "    mov QWORD PTR [rbp%+d], rax\n", off_dst);
        }

        break;
    }

    default:
        fprintf(stderr, "codegen error: unsupported IR instruction %d\n",
                (int)inst->op);
        abort();
    }

    fprintf(out, "\n");
}

static void x64_emit_func(FILE *out, const IrFunc *fn) {
    int frame_size = compute_frame_size(fn);

    fprintf(out, "    .globl %s\n", fn->name);
    fprintf(out, "    .type %s, @function\n", fn->name);
    fprintf(out, "%s:\n", fn->name);

    x64_emit_prologue(out, fn, frame_size);

    for (uint32_t i = 0; i < fn->num_args; i++) {
        int off         = stack_offset_for_value(fn, (IrValue)i);
        const char *reg = ARG_REGS[i];
        fprintf(out, "    mov QWORD PTR [rbp%+d], %s\n", off, reg);
    }
    fprintf(out, "\n");

    for (uint32_t i = 0; i < fn->insts_count; i++) {
        const IrInst *inst = &fn->insts[i];
        x64_emit_inst(out, fn, inst);
    }

    x64_emit_epilogue(out);
}

void x64_emit_module(FILE *out, const IrModule *m) {
    fprintf(out, "    .intel_syntax noprefix\n");
    fprintf(out, "    .text\n\n");

    for (uint32_t i = 0; i < m->funcs_count; i++) {
        const IrFunc *fn = &m->funcs[i];
        x64_emit_func(out, fn);
    }
}
