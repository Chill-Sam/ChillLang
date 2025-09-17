#include "codegen.h"

#include "file_stream.c"
#include "ir.h"

int out_fd = -1;

static void ir_safe_write(const char *buf, int n) {
    int written = 0;
    while (written < n) {
        int w = write(out_fd, buf + written, n - written);
        if (w <= 0) break;
        written += w;
    }
}

static int ir_str_len(const char *s) {
    int i = 0;
    while (s && s[i]) i++;
    return i;
}

static void print_int(int x) {
    char tmp[16];
    int l = itoa64(x, tmp);
    write(out_fd, tmp, l);
}

static int find_slot(const char *name) {
    for (int i = 0; i < slot_count; i++) {
        const char *a = slot_table[i].name, *b = name;
        int j = 0;
        while (a[j] && b[j] && a[j] == b[j]) j++;
        if (a[j] == '\0' && b[j] == '\0') return i;
    }
    return -1;
}

static int add_slot(const char *name) {
    if (slot_count >= MAX_SLOTS) {
        write(STDOUT_FILENO, "ERROR: too many slots\n", 22);
        return -1;
    }
    int idx = slot_count++;
    slot_table[idx].name = (char *)name;
    slot_table[idx].offset = idx * 8;
    return idx;
}

static void build_slots_for_function(const IRFunction *fn) {
    init_slots();

    add_slot("__rbp_saved__");

    for (size_t i = 0; i < fn->n_params; i++) {
        if (find_slot(fn->params[i]) < 0) {
            add_slot(fn->params[i]);
        }
    }

    for (size_t j = 0; j < fn->n_code; j++) {
        IRInst *inst = fn->code[j];

        if (inst->dst) {
            if (find_slot(inst->dst) < 0) {
                add_slot(inst->dst);
            }
        }
    }

    stack_bytes = ((slot_count * 8 + 15) / 16) * 16;
}

void emit_prologue(const IRFunction *fn) {
    // Prologue
    ir_safe_write(".text\n", 6);
    ir_safe_write(".globl ", 7);
    ir_safe_write(fn->name, ir_str_len(fn->name));
    ir_safe_write("\n", 1);
    ir_safe_write(fn->name, ir_str_len(fn->name));
    ir_safe_write(":\n", 2);
    ir_safe_write("  pushq %rbp\n", 13);
    ir_safe_write("  movq %rsp, %rbp\n", 18);
    ir_safe_write("  subq  $", 9);
    print_int(stack_bytes);
    ir_safe_write(", %rsp\n", 7);

    // spill up to first 6 integer arguments

    for (size_t i = 0; i < fn->n_params && i < 6; i++) {
        int idx = find_slot(fn->params[i]);
        if (idx < 0) {
            // slot should always exist by build_slots_for_function
            write(STDOUT_FILENO, "spill arg: unknown slot\n", 24);
            continue;
        }
        int off = slot_table[idx].offset;

        // movq  %rdi, -8(%rbp)   etc.
        ir_safe_write("  movq  ", 8);
        ir_safe_write(arg_regs[i], ir_str_len(arg_regs[i]));
        ir_safe_write(", -", 3);
        print_int(off);
        ir_safe_write("(%rbp)\n", 7);
    }
}

static void lower_inst_to_asm(const IRInst *i) {
    Type *t = i->type;
    int off;

    switch (i->op) {
        case IR_CONST: {
            int idx = find_slot(i->dst);
            if (idx < 0) write(STDOUT_FILENO, "ERR SLOT\n", 9);
            off = slot_table[idx].offset;

            emit_store(t);

            ir_safe_write("$", 1);
            ir_safe_write(i->arg1, ir_str_len(i->arg1));
            ir_safe_write(", -", 3);
            print_int(off);
            ir_safe_write("(%rbp)\n", 7);
            break;
        }

        case IR_LOAD:
        case IR_STORE: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_ADD: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }

            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            ir_safe_write("  addq %rcx, %rax\n", 18);

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_SUB: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }

            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            ir_safe_write("  subq %rcx, %rax\n", 18);

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_MUL: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }

            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            // mul right operand
            if (t->kind == TY_INT) {
                // signed multiply (low 64 bits of RAX ← RAX * RCX)
                ir_safe_write("  imulq %rcx, %rax\n", 19);
            } else {
                // unsigned multiply (low 64 bits of RAX ← RAX * RCX)
                ir_safe_write("  mulq %rcx\n", 12);
            }

            // store result
            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_DIV: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }

            if (t->kind == TY_INT) {
                // sign-extend RAX into RDX
                ir_safe_write("  cqo\n", 6);
            } else {
                // zero RDX for unsigned
                ir_safe_write("  xorq %rdx, %rdx\n", 18);
            }

            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            if (t->kind == TY_INT) {
                ir_safe_write("  idivq %rcx\n", 13);
            } else {
                ir_safe_write("  divq %rcx\n", 12);
            }

            // store result
            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_REM: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }

            if (t->kind == TY_INT) {
                // sign-extend RAX into RDX
                ir_safe_write("  cqo\n", 6);
            } else {
                // zero RDX for unsigned
                ir_safe_write("  xorq %rdx, %rdx\n", 18);
            }

            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            if (t->kind == TY_INT) {
                ir_safe_write("  idivq %rcx\n", 13);
            } else {
                ir_safe_write("  divq %rcx\n", 12);
            }
            ir_safe_write("  movq %rdx, %rax\n", 18);
            // store result
            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_BW_NOT_EXPR: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }

            if (t->size == 1) {
                ir_safe_write("  notb %al\n", 11);
            } else if (t->size == 2) {
                ir_safe_write("  notw %ax\n", 11);
            } else if (t->size == 4) {
                ir_safe_write("  notl %eax\n", 12);
            } else {
                ir_safe_write("  notq %rax\n", 12);
            }

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_SHIFT_RIGHT: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            if (t->size == 1) {
                if (t->kind == TY_INT)
                    ir_safe_write("  sarb %cl, %al\n", 16);
                else
                    ir_safe_write("  shrb %cl, %al\n", 16);
            } else if (t->size == 2) {
                if (t->kind == TY_INT)
                    ir_safe_write("  sarw %cl, %ax\n", 16);
                else
                    ir_safe_write("  shrw %cl, %ax\n", 16);
            } else if (t->size == 4) {
                if (t->kind == TY_INT)
                    ir_safe_write("  sarl %cl, %eax\n", 17);
                else
                    ir_safe_write("  shrl %cl, %eax\n", 17);
            } else {
                if (t->kind == TY_INT)
                    ir_safe_write("  sarq %cl, %rax\n", 17);
                else
                    ir_safe_write("  shrq %cl, %rax\n", 17);
            }

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_SHIFT_LEFT: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            if (t->size == 1) {
                ir_safe_write("  salb %cl, %al\n", 16);
            } else if (t->size == 2) {
                ir_safe_write("  salw %cl, %ax\n", 16);
            } else if (t->size == 4) {
                ir_safe_write("  sall %cl, %eax\n", 17);
            } else {
                ir_safe_write("  salq %cl, %rax\n", 17);
            }

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_BW_AND_EXPR: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            if (t->size == 1) {
                ir_safe_write("  andb %cl, %al\n", 16);
            } else if (t->size == 2) {
                ir_safe_write("  andw %cx, %ax\n", 16);
            } else if (t->size == 4) {
                ir_safe_write("  andl %ecx, %eax\n", 18);
            } else {
                ir_safe_write("  andq %rcx, %rax\n", 18);
            }

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_BW_XOR_EXPR: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            if (t->size == 1) {
                ir_safe_write("  xorb %cl, %al\n", 16);
            } else if (t->size == 2) {
                ir_safe_write("  xorw %cx, %ax\n", 16);
            } else if (t->size == 4) {
                ir_safe_write("  xorl %ecx, %eax\n", 18);
            } else {
                ir_safe_write("  xorq %rcx, %rax\n", 18);
            }

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_BW_OR_EXPR: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
                load_stack_to_reg(i->arg2, "%ecx", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
                load_stack_to_reg(i->arg2, "%rcx", t);
            }

            if (t->size == 1) {
                ir_safe_write("  orb %cl, %al\n", 15);
            } else if (t->size == 2) {
                ir_safe_write("  orw %cx, %ax\n", 15);
            } else if (t->size == 4) {
                ir_safe_write("  orl %ecx, %eax\n", 17);
            } else {
                ir_safe_write("  orq %rcx, %rax\n", 17);
            }

            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_CALL: {
            // move args into registers
            static const char *regs[6] = {"%rdi", "%rsi", "%rdx",
                                          "%rcx", "%r8",  "%r9"};
            for (size_t k = 0; k < i->nargs && k < 6; k++) {
                int so = slot_table[find_slot(i->args[k])].offset;
                ir_safe_write("  movq  -", 9);
                print_int(so);
                ir_safe_write("(%rbp), ", 8);
                ir_safe_write(regs[k], ir_str_len(regs[k]));
                ir_safe_write("\n", 1);
            }
            // call
            ir_safe_write("  call ", 7);
            ir_safe_write(i->arg1, ir_str_len(i->arg1));
            ir_safe_write("\n", 1);
            // move return value into dst
            load_reg_to_stack(t, i->dst);
            break;
        }

        case IR_RET: {
            if (t->size == 4 && t->kind != TY_INT) {
                load_stack_to_reg(i->arg1, "%eax", t);
            } else {
                load_stack_to_reg(i->arg1, "%rax", t);
            }
            break;
        }
        default:
            // no action for other ops
            break;
    }
}

static void lower_ir_const(const IRInst *i) {}

static void emit_load(Type *type) {
    if (type->kind == TY_INT) {
        // Used signed instruciton for INT
        switch (type->size) {
            case 1:  // 8 bit
                ir_safe_write("  movsbq ", 9);
                break;
            case 2:  // 16 bit
                ir_safe_write("  movswq ", 9);
                break;
            case 4:  // 32 bit
                ir_safe_write("  movslq ", 9);
                break;
            default:  // 64 bit
                ir_safe_write("  movq ", 7);
                break;
        }
    } else {
        switch (type->size) {
            case 1:  // 8 bit
                ir_safe_write("  movzbq ", 9);
                break;
            case 2:  // 16 bit
                ir_safe_write("  movzwq ", 9);
                break;
            case 4:  // 32 bit
                ir_safe_write("  movl ", 7);
                break;
            default:  // 64 bit
                ir_safe_write("  movq ", 7);
                break;
        }
    }
}

static void emit_store(Type *type) {
    switch (type->size) {
        case 1:
            ir_safe_write("  movb ", 7);
            break;
        case 2:
            ir_safe_write("  movw ", 7);
            break;
        case 4:
            ir_safe_write("  movl ", 7);
            break;
        default:
            ir_safe_write("  movq ", 7);

            break;
    }
}

static void load_stack_to_reg(const char *from, const char *dst, Type *type) {
    int idx = find_slot(from);
    if (idx < 0) write(STDOUT_FILENO, "load_stack_to_reg: bad from\n", 28);
    int off = slot_table[idx].offset;

    emit_load(type);
    ir_safe_write("-", 1);
    print_int(off);
    ir_safe_write("(%rbp), ", 8);
    ir_safe_write(dst, ir_str_len(dst));
    ir_safe_write("\n", 1);
}

static void load_reg_to_stack(Type *type, const char *dst) {
    if (type->size == 1) {
        ir_safe_write("  movb  %al, -", 14);
    } else if (type->size == 2) {
        ir_safe_write("  movw  %ax, -", 14);
    } else if (type->size == 4) {
        ir_safe_write("  movl  %eax, -", 15);
    } else {
        ir_safe_write("  movq  %rax, -", 15);
    }

    int idx = find_slot(dst);
    if (idx < 0) write(STDOUT_FILENO, "load_reg_to_stack: bad dst\n", 27);
    int off = slot_table[idx].offset;

    print_int(off);
    ir_safe_write("(%rbp)\n", 7);
}

static void emit_start(void) {
    // Emit our own _start:
    ir_safe_write("\n.section .text\n", 16);
    ir_safe_write(".global _start\n", 15);
    ir_safe_write("_start:\n", 8);

    // call main
    ir_safe_write("  call main\n", 12);

    // move main’s return (in RAX) → RDI
    ir_safe_write("  mov  %rax, %rdi\n", 18);

    // syscall number for exit is 60
    ir_safe_write("  mov  $60, %rax\n", 17);

    // do the syscall
    ir_safe_write("  syscall\n", 10);
}
