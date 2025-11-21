#include "x64_codegen.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct FrameLayout {
    int frame_size;
    int *offsets;
} FrameLayout;

static const char *ARG_REGS[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

static int cg_type_size(TypeId tid) {
    const Type *t = type_get(tid);
    switch (t->kind) {
    case TYPE_INT:
    case TYPE_FLOAT:
        return (int)(t->bit_width / 8); // assume 8,16,32,64 only
    case TYPE_BOOL:
        return 1;
    default:
        fprintf(stderr, "codegen: unsupported type kind %d\n", (int)t->kind);
        abort();
    }
}

static int cg_type_is_signed(TypeId tid) {
    const Type *t = type_get(tid);
    if (t->kind != TYPE_INT) {
        fprintf(stderr, "codegen: non-integer type in integer op\n");
        abort();
    }
    return !t->is_unsigned;
}

static int cg_type_align(TypeId tid) {
    int sz = cg_type_size(tid);
    if (sz >= 8)
        return 8;
    if (sz >= 4)
        return 4;
    if (sz >= 2)
        return 2;
    return 1;
}

static int align_up(int x, int align) { return (x + align - 1) & ~(align - 1); }

static FrameLayout cg_build_frame(const IrFunc *fn) {
    FrameLayout fl;
    fl.offsets = malloc(fn->value_count * sizeof(int));
    if (!fl.offsets) {
        fprintf(stderr, "oom in cg_build_frame\n");
        abort();
    }

    int cur = 0; // distance from rbp downwards (positive)
    for (uint32_t v = 0; v < fn->value_count; v++) {
        TypeId tid =
            fn->value_types ? fn->value_types[v] : TYPEID_I64; // fallback
        int sz  = cg_type_size(tid);
        int aln = cg_type_align(tid);

        cur     = align_up(cur, aln);
        cur += sz;
        fl.offsets[v] = -cur; // rbp-relative
    }

    int frame = cur;

    // On entry: rsp%16 == 8, after push rbp: 0, after sub: want 8
    if ((frame % 16) == 0) {
        frame += 8;
    }

    fl.frame_size = frame;
    return fl;
}

static void cg_free_frame(FrameLayout *fl) {
    free(fl->offsets);
    fl->offsets = NULL;
}

static int stack_offset_for_value(const FrameLayout *fl, IrValue v) {
    return fl->offsets[v];
}

static const char *cg_mem_prefix(TypeId tid) {
    int sz = cg_type_size(tid);
    switch (sz) {
    case 1:
        return "BYTE PTR";
    case 2:
        return "WORD PTR";
    case 4:
        return "DWORD PTR";
    case 8:
        return "QWORD PTR";
    default:
        fprintf(stderr, "codegen: unsupported type size %d\n", sz);
        abort();
    }
}

static const char *cg_reg_for_type(TypeId tid) {
    int sz = cg_type_size(tid);
    switch (sz) {
    case 1:
        return "al";
    case 2:
        return "ax";
    case 4:
        return "eax";
    case 8:
        return "rax";
    default:
        fprintf(stderr, "codegen: unsupported type size %d\n", sz);
        abort();
    }
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

static void x64_emit_div_or_mod(FILE *out, const FrameLayout *fl,
                                const IrFunc *fn, const IrInst *inst,
                                int is_mod) {
    TypeId t      = inst->type;
    int sz        = cg_type_size(t);
    int is_signed = cg_type_is_signed(t);

    int div_sz    = (sz <= 4) ? 4 : 8;

    int off_dst   = stack_offset_for_value(fl, inst->dst);
    int off0      = stack_offset_for_value(fl, inst->src0);
    int off1      = stack_offset_for_value(fl, inst->src1);

    if (div_sz == 4) {
        const char *div_mem = "DWORD PTR";

        // Load dividend into EAX
        fprintf(out, "    mov eax, %s [rbp%+d]\n", div_mem, off0);

        if (is_signed) {
            fprintf(out, "    cdq\n"); // sign-extend eax -> edx:eax
            fprintf(out, "    idiv %s [rbp%+d]\n", div_mem, off1);
        } else {
            fprintf(out, "    xor edx, edx\n");
            fprintf(out, "    div %s [rbp%+d]\n", div_mem, off1);
        }

        // Now quotient in EAX, remainder in EDX (32-bit)
        const char *store_mem =
            cg_mem_prefix(t); // BYTE/WORD/DWORD for final type
        if (is_mod) {
            fprintf(out, "    mov %s [rbp%+d], edx\n", store_mem, off_dst);
        } else {
            fprintf(out, "    mov %s [rbp%+d], eax\n", store_mem, off_dst);
        }
    } else { // div_sz == 8
        const char *div_mem = "QWORD PTR";

        fprintf(out, "    mov rax, %s [rbp%+d]\n", div_mem, off0);

        if (is_signed) {
            fprintf(out, "    cqo\n"); // sign-extend rax -> rdx:rax
            fprintf(out, "    idiv %s [rbp%+d]\n", div_mem, off1);
        } else {
            fprintf(out, "    xor rdx, rdx\n");
            fprintf(out, "    div %s [rbp%+d]\n", div_mem, off1);
        }

        // quotient in RAX, remainder in RDX (64-bit)
        const char *store_mem = cg_mem_prefix(t); // may be QWORD
        if (is_mod) {
            fprintf(out, "    mov %s [rbp%+d], rdx\n", store_mem, off_dst);
        } else {
            fprintf(out, "    mov %s [rbp%+d], rax\n", store_mem, off_dst);
        }
    }
}

static void x64_emit_inst(FILE *out, const IrFunc *fn, const FrameLayout *fl,
                          const IrInst *inst) {
    // TODO: Implement other instructions
    switch (inst->op) {
    case IR_CONST_INT: {
        TypeId t        = inst->type;
        const char *reg = cg_reg_for_type(t);
        const char *mem = cg_mem_prefix(t);

        int off_dst     = stack_offset_for_value(fl, inst->dst);
        fprintf(out, "    mov %s [rbp%+d], %lld\n", mem, off_dst,
                (long long)inst->imm);
        break;
    }

    case IR_OP_ADD:
    case IR_OP_SUB:
    case IR_OP_MUL: {
        TypeId t        = inst->type;
        const char *reg = cg_reg_for_type(t);
        const char *mem = cg_mem_prefix(t);

        int off_dst     = stack_offset_for_value(fl, inst->dst);
        int off0        = stack_offset_for_value(fl, inst->src0);
        int off1        = stack_offset_for_value(fl, inst->src1);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, mem, off0);

        switch (inst->op) {
        case IR_OP_ADD:
            fprintf(out, "    add %s, %s [rbp%+d]\n", reg, mem, off1);
            break;
        case IR_OP_SUB:
            fprintf(out, "    sub %s, %s [rbp%+d]\n", reg, mem, off1);
            break;
        case IR_OP_MUL:
            fprintf(out, "    imul %s, %s [rbp%+d]\n", reg, mem, off1);
            break;
        default:
            break;
        }

        fprintf(out, "    mov %s [rbp%+d], %s\n", mem, off_dst, reg);
        break;
    }

    case IR_OP_DIV:
    case IR_OP_MOD: {
        x64_emit_div_or_mod(out, fl, fn, inst, inst->op == IR_OP_MOD);
        break;
    }

    case IR_OP_AND:
    case IR_OP_OR:
    case IR_OP_XOR: {
        TypeId t        = inst->type;
        const char *reg = cg_reg_for_type(t);
        const char *mem = cg_mem_prefix(t);

        int off_dst     = stack_offset_for_value(fl, inst->dst);
        int off0        = stack_offset_for_value(fl, inst->src0);
        int off1        = stack_offset_for_value(fl, inst->src1);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, mem, off0);
        switch (inst->op) {
        case IR_OP_AND:
            fprintf(out, "    and %s, %s [rbp%+d]\n", reg, mem, off1);
            break;
        case IR_OP_OR:
            fprintf(out, "    or %s, %s [rbp%+d]\n", reg, mem, off1);
            break;
        case IR_OP_XOR:
            fprintf(out, "    xor %s, %s [rbp%+d]\n", reg, mem, off1);
            break;
        default:
            fprintf(stderr, "codegen error: unreachable code\n");
            abort();
        }

        fprintf(out, "    mov %s [rbp%+d], %s\n", mem, off_dst, reg);
        break;
    }

    case IR_OP_SHL:
    case IR_OP_SHR: {
        TypeId t        = inst->type;
        const char *reg = cg_reg_for_type(t);
        const char *mem = cg_mem_prefix(t);

        int off_dst     = stack_offset_for_value(fl, inst->dst);
        int off0        = stack_offset_for_value(fl, inst->src0);
        int off1        = stack_offset_for_value(fl, inst->src1);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, mem, off0);
        fprintf(out, "    mov rcx, QWORD PTR [rbp%+d]\n",
                off1); // Keeps shift count as 64 bit
        switch (inst->op) {
        case IR_OP_SHL:
            fprintf(out, "    shl %s, cl\n", reg);
            break;
        case IR_OP_SHR:
            fprintf(out, "    shr %s, cl\n", reg);
            break;
        default:
            fprintf(stderr, "codegen error: unreachable code\n");
            abort();
        }

        fprintf(out, "    mov %s [rbp%+d], %s\n", mem, off_dst, reg);
        break;
    }

    case IR_OP_NEG:
    case IR_OP_BITNOT: {
        TypeId t        = inst->type;
        const char *reg = cg_reg_for_type(t);
        const char *mem = cg_mem_prefix(t);

        int off_dst     = stack_offset_for_value(fl, inst->dst);
        int off0        = stack_offset_for_value(fl, inst->src0);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, mem, off0);
        switch (inst->op) {
        case IR_OP_NEG:
            fprintf(out, "    neg %s\n", reg);
            break;
        case IR_OP_BITNOT:
            fprintf(out, "    not %s\n", reg);
            break;
        default:
            fprintf(stderr, "codegen error: unreachable code\n");
            abort();
        }

        fprintf(out, "    mov %s [rbp%+d], %s\n", mem, off_dst, reg);
        break;
    }

    case IR_OP_ZEXT:
    case IR_OP_SEXT:
    case IR_OP_TRUNC: {
        TypeId dst_t        = inst->type;
        TypeId src_t        = fn->value_types[inst->src0];

        int dst_sz          = cg_type_size(dst_t);
        int src_sz          = cg_type_size(src_t);

        int off_dst         = stack_offset_for_value(fl, inst->dst);
        int off_src         = stack_offset_for_value(fl, inst->src0);

        const char *dst_mem = cg_mem_prefix(dst_t);
        const char *src_mem = cg_mem_prefix(src_t);

        if (inst->op == IR_OP_TRUNC) {
            const char *reg = cg_reg_for_type(dst_t);
            fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, dst_mem, off_src);
            fprintf(out, "    mov %s [rbp%+d], %s\n", dst_mem, off_dst, reg);
            break;
        }

        if (src_sz != 1 && src_sz != 2 && src_sz != 4) {
            fprintf(stderr,
                    "codegen error: unsupported type size %d in ZEXT/SEXT \n",
                    src_sz);
            abort();
        }

        const char *cmd = src_sz == 4              ? "mov"
                          : inst->op == IR_OP_ZEXT ? "movzx"
                                                   : "movsx";
        fprintf(out, "    %s eax, %s [rbp%+d]\n", cmd, src_mem, off_src);
        if (src_sz == 4 && dst_sz == 8 && inst->op == IR_OP_SEXT) {
            fprintf(out, "    cdqe\n");
        }

        const char *dst_reg = cg_reg_for_type(dst_t);
        fprintf(out, "    mov %s [rbp%+d], %s\n", dst_mem, off_dst, dst_reg);
        break;
    }

    case IR_OP_MOV: {
        TypeId t        = inst->type;
        const char *reg = cg_reg_for_type(t);
        const char *mem = cg_mem_prefix(t);

        int off_dst     = stack_offset_for_value(fl, inst->dst);
        int off_src     = stack_offset_for_value(fl, inst->src0);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, mem, off_src);
        fprintf(out, "    mov %s [rbp%+d], %s\n", mem, off_dst, reg);
        break;
    }

    case IR_OP_CALL: {
        for (uint8_t i = 0; i < inst->call_arg_count; i++) {
            IrValue arg_v   = inst->call_args[i];
            int off         = stack_offset_for_value(fl, arg_v);
            const char *reg = ARG_REGS[i];
            fprintf(out, "    mov %s, QWORD PTR [rbp%+d]\n", reg, off);
        }

        fprintf(out, "    call %s\n", inst->call_name);

        if (inst->dst != (IrValue)~0u) {
            TypeId t        = inst->type;
            const char *mem = cg_mem_prefix(t);
            const char *reg = cg_reg_for_type(t);

            int off_dst     = stack_offset_for_value(fl, inst->dst);
            fprintf(out, "    mov %s [rbp%+d], %s\n", mem, off_dst, reg);
        }

        break;
    }

    case IR_OP_RET: {
        if (inst->src0 != (IrValue)~0u) {
            TypeId t        = inst->type;
            const char *reg = cg_reg_for_type(t);
            const char *mem = cg_mem_prefix(t);

            int off         = stack_offset_for_value(fl, inst->src0);
            fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, mem, off);
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
    FrameLayout fl = cg_build_frame(fn);

    fprintf(out, "    .globl %s\n", fn->name);
    fprintf(out, "    .type %s, @function\n", fn->name);
    fprintf(out, "%s:\n", fn->name);

    x64_emit_prologue(out, fn, fl.frame_size);

    for (uint32_t i = 0; i < fn->num_args; i++) {
        TypeId t        = fn->value_types[i];
        const char *reg = ARG_REGS[i];
        int off         = stack_offset_for_value(&fl, (IrValue)i);
        fprintf(out, "    mov QWORD PTR [rbp%+d], %s\n", off, reg);
    }
    fprintf(out, "\n");

    for (uint32_t i = 0; i < fn->insts_count; i++) {
        const IrInst *inst = &fn->insts[i];
        x64_emit_inst(out, fn, &fl, inst);
    }

    x64_emit_epilogue(out);
    cg_free_frame(&fl);
}

void x64_emit_module(FILE *out, const IrModule *m) {
    fprintf(out, "    .intel_syntax noprefix\n");
    fprintf(out, "    .text\n\n");

    for (uint32_t i = 0; i < m->funcs_count; i++) {
        const IrFunc *fn = &m->funcs[i];
        x64_emit_func(out, fn);
    }
}
