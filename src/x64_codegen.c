#include "x64_codegen.h"
#include <stdlib.h>

typedef struct FrameLayout {
    int frame_size;
    int *alloca_data_off;
    int *value_slot_off;
} FrameLayout;

typedef struct CgSizeInfo {
    const char *mem;
    const char *reg;
} CgSizeInfo;

static const char *ARG_REGS_8[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
static const char *ARG_REGS_4[6] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static const char *ARG_REGS_2[6] = {"di", "si", "dx", "cx", "r8w", "r9w"};
static const char *ARG_REGS_1[6] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};

// ----- Codegen type helper functions -----
static int cg_type_size(TypeId tid) {
    const Type *t = type_get(tid);
    switch (t->kind) {
    case TYPE_INT:
    case TYPE_FLOAT:
    case TYPE_STRUCT:
    case TYPE_PTR:
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

static const char *cg_get_arg_reg(int arg_count, TypeId type) {
    int sz = cg_type_size(type);
    switch (sz) {
    case 1:
        return ARG_REGS_1[arg_count];
    case 2:
        return ARG_REGS_2[arg_count];
    case 4:
        return ARG_REGS_4[arg_count];
    case 8:
        return ARG_REGS_8[arg_count];
    default:
        fprintf(stderr, "codegen: unsupported type size %d\n", sz);
        abort();
    }
}

// Calculate size information for a type.
static CgSizeInfo cg_size_info(TypeId tid) {
    int sz = cg_type_size(tid);

    CgSizeInfo size_info;

    switch (sz) {
    case 1: {
        size_info.reg = "al";
        size_info.mem = "BYTE PTR";
        break;
    }
    case 2: {
        size_info.reg = "ax";
        size_info.mem = "WORD PTR";
        break;
    }
    case 4: {
        size_info.reg = "eax";
        size_info.mem = "DWORD PTR";
        break;
    }
    case 8: {
        size_info.reg = "rax";
        size_info.mem = "QWORD PTR";
        break;
    }
    default:
        fprintf(stderr, "codegen: unsupported type size %d\n", sz);
        abort();
    }

    return size_info;
}

// Codegen frame layout calculation
static int align_up(int x, int align) { return (x + align - 1) & ~(align - 1); }

static FrameLayout cg_build_frame(const IrFunc *fn) {
    FrameLayout fl;
    fl.alloca_data_off = malloc(fn->value_count * sizeof(int));
    fl.value_slot_off  = malloc(fn->value_count * sizeof(int));
    if (!fl.alloca_data_off || !fl.value_slot_off) {
        fprintf(stderr, "oom in cg_build_frame\n");
        abort();
    }

    for (uint32_t i = 0; i < fn->value_count; i++) {
        fl.alloca_data_off[i] = 0;
        fl.value_slot_off[i]  = 0;
    }

    int cur = 0; // distance from rbp downwards (positive)

    // Allocate for ALLOCAs
    for (IrBlock *b = fn->entry; b; b = b->next) {
        for (IrInst *inst = b->first; inst; inst = inst->next) {
            if (inst->op != IR_OP_ALLOCA)
                continue;

            int sz  = cg_type_size(inst->type);
            int aln = cg_type_align(inst->type);
            cur     = align_up(cur, aln);
            cur += sz;
            fl.alloca_data_off[inst->dst] = -cur; // rbp-relative
        }
    }

    for (uint32_t v = 0; v < fn->value_count; v++) {
        int sz  = fl.alloca_data_off[v] != 0
                      ? 8
                      : cg_type_size(fn->value_types ? fn->value_types[v]
                                                     : TYPEID_I64);
        int aln = fl.alloca_data_off[v] != 0
                      ? 8
                      : cg_type_align(fn->value_types ? fn->value_types[v]
                                                      : TYPEID_I64);

        cur     = align_up(cur, aln);
        cur += sz;
        fl.value_slot_off[v] = -cur; // rbp-relative
    }

    fl.frame_size = align_up(cur, 16);
    return fl;
}

static void cg_free_frame(FrameLayout *fl) {
    free(fl->alloca_data_off);
    free(fl->value_slot_off);
    fl->alloca_data_off = NULL;
    fl->value_slot_off  = NULL;
}

static int stack_offset_for_value(const FrameLayout *fl, IrValue v) {
    return fl->value_slot_off[v];
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
                                const IrInst *inst, int is_mod) {
    TypeId t              = inst->type;
    int is_signed         = cg_type_is_signed(t);

    int off_dst           = stack_offset_for_value(fl, inst->dst);
    int off_src0          = stack_offset_for_value(fl, inst->src0);
    int off_src1          = stack_offset_for_value(fl, inst->src1);

    bool wide             = (cg_type_size(t) > 4);
    const char *acc       = wide ? "rax" : "eax";
    const char *remainder = wide ? "rdx" : "edx";
    const char *div_mem   = wide ? "QWORD PTR" : "DWORD PTR";
    const char *sext      = wide ? "cqo" : "cdq";

    fprintf(out, "    mov %s, %s [rbp%+d]\n", acc, div_mem, off_src0);
    if (is_signed) {
        fprintf(out, "    %s\n", sext); // sign-extend
        fprintf(out, "    idiv %s [rbp%+d]\n", div_mem, off_src1);
    } else {
        fprintf(out, "    xor %s, %s\n", remainder, remainder);
        fprintf(out, "    div %s [rbp%+d]\n", div_mem, off_src1);
    }

    // Now quotient in EAX (32-bit) or RAX (64-bit), remainder in EDX (32-bit)
    // or RDX (64-bit)
    CgSizeInfo size_info = cg_size_info(t);
    if (is_mod) {
        fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst,
                remainder);
    } else {
        fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst, acc);
    }
}

// Handles SUB, MUL, AND, OR, XOR, LOGICAL_AND and LOGICAL_OR
static const char *norm_binop_mnem(IrOp op) {
    // LOGICAL_AND/LOGICAL_OR are the same as AND/OR since lowering guarantees
    // bool types
    switch (op) {
    case IR_OP_SUB:
        return "sub";
    case IR_OP_MUL:
        return "imul";
    case IR_OP_AND:
    case IR_OP_LOGICAL_AND:
        return "and";
    case IR_OP_OR:
    case IR_OP_LOGICAL_OR:
        return "or";
    case IR_OP_XOR:
        return "xor";
    default:
        fprintf(stderr,
                "codegen error: invalid operator %d given to norm_binop_mnem\n",
                op);
        abort();
    }
}

// Handles SUB, MUL, AND, OR, XOR, LOGICAL_AND and LOGICAL_OR
static void x64_emit_norm_binop(FILE *out, const FrameLayout *fl,
                                const IrInst *inst) {
    TypeId t             = inst->type;
    CgSizeInfo size_info = cg_size_info(t);

    int off_dst          = stack_offset_for_value(fl, inst->dst);
    int off_src0         = stack_offset_for_value(fl, inst->src0);
    int off_src1         = stack_offset_for_value(fl, inst->src1);

    fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg, size_info.mem,
            off_src0);

    const char *mnem = norm_binop_mnem(inst->op);
    fprintf(out, "    %s %s, %s [rbp%+d]\n", mnem, size_info.reg, size_info.mem,
            off_src1);

    fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst,
            size_info.reg);
}

// Handles NEG and BITNOT
static const char *unary_mnem(IrOp op) {
    switch (op) {
    case IR_OP_NEG:
        return "neg";
    case IR_OP_BITNOT:
        return "not";
    default:
        fprintf(stderr,
                "codegen error: invalid operator %d given to unary_mnem\n", op);
        abort();
    }
}

static void x64_emit_unary_op(FILE *out, const FrameLayout *fl,
                              const IrInst *inst) {
    TypeId t             = inst->type;
    CgSizeInfo size_info = cg_size_info(t);

    int off_dst          = stack_offset_for_value(fl, inst->dst);
    int off0             = stack_offset_for_value(fl, inst->src0);

    fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg, size_info.mem,
            off0);

    const char *mnem = unary_mnem(inst->op);
    fprintf(out, "    %s %s\n", mnem, size_info.reg);

    fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst,
            size_info.reg);
}

static void x64_emit_inst(FILE *out, const IrFunc *fn, const FrameLayout *fl,
                          const IrInst *inst) {
    // TODO: Implement other instructions
    switch (inst->op) {
    case IR_CONST_INT: {
        TypeId t             = inst->type;
        CgSizeInfo size_info = cg_size_info(t);

        int off_dst          = stack_offset_for_value(fl, inst->dst);
        fprintf(out, "    mov %s [rbp%+d], %lld\n", size_info.mem, off_dst,
                (long long)inst->imm);
        break;
    }

    case IR_OP_ADD: {
        TypeId t             = inst->type;
        CgSizeInfo size_info = cg_size_info(t);

        int off_dst          = stack_offset_for_value(fl, inst->dst);
        int off0             = stack_offset_for_value(fl, inst->src0);
        int off1             = stack_offset_for_value(fl, inst->src1);

        if (t == TYPEID_PTR) {
            // Pointer arithmetic
            fprintf(out, "    mov rax, QWORD PTR [rbp%+d]\n", off0);
            fprintf(out, "    mov rcx, QWORD PTR [rbp%+d]\n", off1);
            fprintf(out, "    add rax, rcx\n");
            fprintf(out, "    mov QWORD PTR [rbp%+d], rax\n", off_dst);
        } else {
            // Normal integer add
            fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg,
                    size_info.mem, off0);
            fprintf(out, "    add %s, %s [rbp%+d]\n", size_info.reg,
                    size_info.mem, off1);
            fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst,
                    size_info.reg);
        }
        break;
    }

    case IR_OP_SUB:
    case IR_OP_MUL:
    case IR_OP_AND:
    case IR_OP_OR:
    case IR_OP_XOR:
    case IR_OP_LOGICAL_AND:
    case IR_OP_LOGICAL_OR: {
        x64_emit_norm_binop(out, fl, inst);
        break;
    }

    case IR_OP_DIV:
    case IR_OP_MOD: {
        x64_emit_div_or_mod(out, fl, inst, inst->op == IR_OP_MOD);
        break;
    }

    case IR_OP_SHL:
    case IR_OP_SHR: {
        TypeId t             = inst->type;
        CgSizeInfo size_info = cg_size_info(t);

        int off_dst          = stack_offset_for_value(fl, inst->dst);
        int off0             = stack_offset_for_value(fl, inst->src0);
        int off1             = stack_offset_for_value(fl, inst->src1);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg, size_info.mem,
                off0);
        fprintf(out, "    mov rcx, QWORD PTR [rbp%+d]\n",
                off1); // Keeps shift count as 64 bit
        switch (inst->op) {
        case IR_OP_SHL:
            fprintf(out, "    shl %s, cl\n", size_info.reg);
            break;
        case IR_OP_SHR:
            fprintf(out, "    shr %s, cl\n", size_info.reg);
            break;
        default:
            fprintf(stderr, "codegen error: unreachable code\n");
            abort();
        }

        fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst,
                size_info.reg);
        break;
    }

    case IR_OP_CMP_LT:
    case IR_OP_CMP_LE:
    case IR_OP_CMP_GT:
    case IR_OP_CMP_GE:
    case IR_OP_CMP_EQ:
    case IR_OP_CMP_NE: {
        TypeId t             = fn->value_types[inst->src0];
        CgSizeInfo size_info = cg_size_info(t);

        int off_dst          = stack_offset_for_value(fl, inst->dst);
        int off_src0         = stack_offset_for_value(fl, inst->src0);
        int off_src1         = stack_offset_for_value(fl, inst->src1);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg, size_info.mem,
                off_src0);
        fprintf(out, "    cmp %s, %s [rbp%+d]\n", size_info.reg, size_info.mem,
                off_src1);

        const Type *lt  = type_get(t);
        int is_unsigned = lt->is_unsigned;

        const char *cc  = NULL;
        switch (inst->op) {
        case IR_OP_CMP_EQ:
            cc = "e"; // equal
            break;
        case IR_OP_CMP_NE:
            cc = "ne"; // not equal
            break;
        case IR_OP_CMP_LT:
            cc = is_unsigned ? "b" : "l"; // below / less
            break;
        case IR_OP_CMP_LE:
            cc = is_unsigned ? "be" : "le"; // below or equal / less or equal
            break;
        case IR_OP_CMP_GT:
            cc = is_unsigned ? "a" : "g"; // above / greater
            break;
        case IR_OP_CMP_GE:
            cc = is_unsigned ? "ae" : "ge"; // above or equal / greater or equal
            break;
        default:
            fprintf(stderr, "codegen: unexpected cmp op %d\n", inst->op);
            abort();
        }

        // setcc writes a 0/1 byte into AL; bool is stored as 1 byte
        CgSizeInfo bool_size_info = cg_size_info(inst->type);
        fprintf(out, "    set%s al\n", cc);
        fprintf(out, "    mov %s [rbp%+d], al\n", bool_size_info.mem, off_dst);

        break;
    }

    case IR_OP_NEG:
    case IR_OP_BITNOT: {
        x64_emit_unary_op(out, fl, inst);
        break;
    }

    case IR_OP_ZEXT:
    case IR_OP_SEXT:
    case IR_OP_TRUNC: {
        TypeId dst_t             = inst->type;
        TypeId src_t             = fn->value_types[inst->src0];

        int dst_sz               = cg_type_size(dst_t);
        int src_sz               = cg_type_size(src_t);

        int off_dst              = stack_offset_for_value(fl, inst->dst);
        int off_src              = stack_offset_for_value(fl, inst->src0);

        CgSizeInfo dst_size_info = cg_size_info(dst_t);
        CgSizeInfo src_size_info = cg_size_info(src_t);

        if (inst->op == IR_OP_TRUNC) {
            fprintf(out, "    mov %s, %s [rbp%+d]\n", dst_size_info.reg,
                    dst_size_info.mem, off_src);
            fprintf(out, "    mov %s [rbp%+d], %s\n", dst_size_info.mem,
                    off_dst, dst_size_info.reg);
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
        fprintf(out, "    %s eax, %s [rbp%+d]\n", cmd, src_size_info.mem,
                off_src);
        if (src_sz == 4 && dst_sz == 8 && inst->op == IR_OP_SEXT) {
            fprintf(out, "    cdqe\n");
        }

        fprintf(out, "    mov %s [rbp%+d], %s\n", dst_size_info.mem, off_dst,
                dst_size_info.reg);
        break;
    }

    case IR_OP_ALLOCA: {
        int data_off = fl->alloca_data_off[inst->dst];
        int ptr_off  = fl->value_slot_off[inst->dst];
        fprintf(out, "    lea rax, [rbp%+d]\n", data_off);
        fprintf(out, "    mov QWORD PTR [rbp%+d], rax\n", ptr_off);
        break;
    }

    case IR_OP_LOAD: {
        TypeId t             = inst->type;
        CgSizeInfo size_info = cg_size_info(t);

        int src_off          = stack_offset_for_value(fl, inst->src0);
        int dst_off          = stack_offset_for_value(fl, inst->dst);

        fprintf(out, "    mov rbx, QWORD PTR [rbp%+d]\n", src_off);
        fprintf(out, "    mov %s, %s [rbx]\n", size_info.reg, size_info.mem);
        fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, dst_off,
                size_info.reg);
        break;
    }

    case IR_OP_STORE: {
        TypeId t             = inst->type;
        CgSizeInfo size_info = cg_size_info(t);

        int dst_off          = stack_offset_for_value(fl, inst->dst);
        int src_off          = stack_offset_for_value(fl, inst->src0);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg, size_info.mem,
                src_off);
        fprintf(out, "    mov rbx, QWORD PTR [rbp%+d]\n", dst_off);
        fprintf(out, "    mov %s [rbx], %s\n", size_info.mem, size_info.reg);
        break;
    }

    case IR_OP_MOV: {
        TypeId t             = inst->type;
        CgSizeInfo size_info = cg_size_info(t);

        int off_dst          = stack_offset_for_value(fl, inst->dst);
        int off_src          = stack_offset_for_value(fl, inst->src0);

        fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg, size_info.mem,
                off_src);
        fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst,
                size_info.reg);
        break;
    }

    case IR_OP_LABEL:
        fprintf(out, ".L%s%ld:\n", fn->name, inst->imm);
        break;

    case IR_OP_BR:
        fprintf(out, "    jmp .L%s%ld\n", fn->name, inst->imm);
        break;

    case IR_OP_BRCOND: {
        TypeId cond_t             = fn->value_types[inst->src0];
        CgSizeInfo cond_size_info = cg_size_info(cond_t);

        IrValue cond              = inst->src0;
        int off                   = stack_offset_for_value(fl, cond);
        fprintf(out, "    cmp %s [rbp%+d], 0\n", cond_size_info.mem, off);
        fprintf(out, "    jne .L%s%ld\n", fn->name, inst->imm);
        break;
    }

    case IR_OP_PHI:
        fprintf(stderr, "codegen error: unexpected IR_OP_PHI\n");
        abort();

    case IR_OP_CALL: {
        for (uint8_t i = 0; i < inst->call_arg_count; i++) {
            IrValue arg_v            = inst->call_args[i];
            TypeId arg_t             = fn->value_types[arg_v];
            CgSizeInfo arg_size_info = cg_size_info(arg_t);

            int off                  = stack_offset_for_value(fl, arg_v);

            const char *reg          = cg_get_arg_reg(i, arg_t);
            fprintf(out, "    mov %s, %s [rbp%+d]\n", reg, arg_size_info.mem,
                    off);
        }

        fprintf(out, "    call %s\n", inst->call_name);

        if (inst->dst != IR_VALUE_NONE) {
            TypeId t             = inst->type;
            CgSizeInfo size_info = cg_size_info(t);

            int off_dst          = stack_offset_for_value(fl, inst->dst);
            fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off_dst,
                    size_info.reg);
        }

        break;
    }

    case IR_OP_RET: {
        if (inst->src0 != IR_VALUE_NONE) {
            TypeId t             = inst->type;
            CgSizeInfo size_info = cg_size_info(t);

            int off              = stack_offset_for_value(fl, inst->src0);
            fprintf(out, "    mov %s, %s [rbp%+d]\n", size_info.reg,
                    size_info.mem, off);
            fprintf(out, "    jmp .L%s_exit\n", fn->name);
        }

        break;
    }

    default:
        fprintf(stderr, "codegen error: unsupported IR instruction %d\n",
                (int)inst->op);
        abort();
    }
}

static void x64_emit_func(FILE *out, const IrFunc *fn) {
    FrameLayout fl = cg_build_frame(fn);

    fprintf(out, "    .globl %s\n", fn->name);
    fprintf(out, "    .type %s, @function\n", fn->name);
    fprintf(out, "%s:\n", fn->name);

    x64_emit_prologue(out, fn, fl.frame_size);

    for (uint32_t i = 0; i < fn->num_args; i++) {
        TypeId t             = fn->value_types[i];
        const char *reg      = cg_get_arg_reg(i, t);
        CgSizeInfo size_info = cg_size_info(t);
        int off              = stack_offset_for_value(&fl, (IrValue)i);

        fprintf(out, "    mov %s [rbp%+d], %s\n", size_info.mem, off, reg);
    }

    IrBlock *block = fn->entry;
    for (uint32_t i = 0; i < fn->block_count; i++) {
        IrInst *cur_inst = block->first;
        while (cur_inst != block->last) {
            x64_emit_inst(out, fn, &fl, cur_inst);
            cur_inst = cur_inst->next;
        }
        x64_emit_inst(out, fn, &fl, cur_inst); // emit last instruction

        block = block->next;
    }

    fprintf(out, ".L%s_exit:\n", fn->name);
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
