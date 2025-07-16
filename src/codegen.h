#pragma once

#include "AST/ast_log.c"
#include "file_stream.c"
#include "ir.h"

#define MAX_SLOTS 256

// A simple name→offset map
typedef struct {
    char *name; // pointer into your arena or strdup’d name
    int offset; // byte offset from %rbp: 8,16,24,...
} Slot;

static const char *arg_regs[6] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};

static Slot slot_table[MAX_SLOTS];
static int slot_count;
static int stack_bytes;

static void init_slots(void) { slot_count = 0; }

static int find_slot(const char *name);
static int add_slot(const char *name);
static void build_slots_for_function(const IRFunction *fn);

void emit_prologue(const IRFunction *fn);
static void lower_inst_to_asm(const IRInst *i);
