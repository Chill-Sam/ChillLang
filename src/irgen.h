#pragma once

#include "AST/ast_arena.c"
#include "AST/ast_log.c"
#include "ir.h"
#include "ir_type.h"
#include "symtab.h"

static int temp_counter = 0;
static int label_counter = 0;

static char *make_name(char prefix);
static char *new_temp(void);
static char *new_label(void);

static IRInst *make_inst(void);
static void ir_emit(IRFunction *fn, IRInst *inst);
static char *lower_expr(ASTNode *expr, IRFunction *fn);
static void lower_stmt(ASTNode *stmt, IRFunction *fn);
void irgen_program(ASTNode *ast_root, IRProgram *pr);
