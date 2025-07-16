// ir_print.c
#pragma once

#include "file_stream.c"
#include "int.c"
#include "ir.h"  // your IR structs

// ————————————————————————————————————————————————————————————————
// Helpers: string length and safe write
// ————————————————————————————————————————————————————————————————
static int ir_print_str_len(const char *s) {
    int i = 0;
    while (s && s[i]) i++;
    return i;
}

static void ir_print_safe_write(const char *buf, int n) {
    int written = 0;
    while (written < n) {
        int w = write(STDOUT_FILENO, buf + written, n - written);
        if (w <= 0) break;
        written += w;
    }
}

// ————————————————————————————————————————————————————————————————
// Print a single IR instruction
// ————————————————————————————————————————————————————————————————
static void ir_print_inst(const IRInst *i) {
    switch (i->op) {
        case IR_CONST:
            //   dst = const arg1
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = const ", 9);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_LOAD:
            //   dst = load arg1
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = load ", 8);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_STORE:
            //   store arg1 -> arg2
            ir_print_safe_write("  store ", 8);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" -> ", 4);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_ADD:
            //   dst = arg1 + arg2
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = ", 3);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" + ", 3);
            ir_print_safe_write(i->arg2, ir_print_str_len(i->arg2));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_SUB:
            //   dst = arg1 + arg2
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = ", 3);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" - ", 3);
            ir_print_safe_write(i->arg2, ir_print_str_len(i->arg2));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_MUL:
            //   dst = arg1 + arg2
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = ", 3);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" * ", 3);
            ir_print_safe_write(i->arg2, ir_print_str_len(i->arg2));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_DIV:
            //   dst = arg1 + arg2
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = ", 3);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" / ", 3);
            ir_print_safe_write(i->arg2, ir_print_str_len(i->arg2));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_REM:
            //   dst = arg1 + arg2
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = ", 3);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" % ", 3);
            ir_print_safe_write(i->arg2, ir_print_str_len(i->arg2));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_CALL:
            //   dst = call fn(arg0, arg1, …)
            ir_print_safe_write("  ", 2);
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(" = call ", 8);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write("(", 1);
            for (size_t k = 0; k < i->nargs; k++) {
                if (k > 0) ir_print_safe_write(", ", 2);
                ir_print_safe_write(i->args[k], ir_print_str_len(i->args[k]));
            }
            ir_print_safe_write(")", 1);
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_RET:
            //   return arg1
            ir_print_safe_write("  return ", 9);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write(" type = ", 8);
            ir_print_safe_write(i->type->name, ir_print_str_len(i->type->name));
            ir_print_safe_write("\n", 1);
            break;

        case IR_LABEL:
            // label:
            ir_print_safe_write(i->dst, ir_print_str_len(i->dst));
            ir_print_safe_write(":\n", 2);
            break;

        case IR_BR:
            //   br arg1
            ir_print_safe_write("  br ", 4);
            ir_print_safe_write(i->arg1, ir_print_str_len(i->arg1));
            ir_print_safe_write("\n", 1);
            break;

        default:
            ir_print_safe_write("  <unknown IR opcode>\n", 23);
            break;
    }
}

// ————————————————————————————————————————————————————————————————
// Print one function (header + its instructions)
// ————————————————————————————————————————————————————————————————
static void ir_print_function(const IRFunction *fn) {
    // func NAME(param0, param1, …)
    ir_print_safe_write("func ", 5);
    ir_print_safe_write(fn->name, ir_print_str_len(fn->name));
    ir_print_safe_write("(", 1);
    for (size_t i = 0; i < fn->n_params; i++) {
        if (i > 0) ir_print_safe_write(", ", 2);
        ir_print_safe_write(fn->params[i], ir_print_str_len(fn->params[i]));
    }
    ir_print_safe_write(")\n", 2);

    // each instruction
    for (size_t j = 0; j < fn->n_code; j++) {
        ir_print_inst(fn->code[j]);
    }
    ir_print_safe_write("\n", 1);
}

// ————————————————————————————————————————————————————————————————
// Public entrypoint: print an entire IRProgram
// ————————————————————————————————————————————————————————————————
void ir_print_program(const IRProgram *pr) {
    for (size_t f = 0; f < pr->n_funcs; f++) {
        ir_print_function(pr->funcs[f]);
    }
}
