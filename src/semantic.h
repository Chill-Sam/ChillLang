#pragma once

#include "AST/ast.c"
#include "AST/ast_builder.c"
#include "AST/ast_log.c"
#include "symtab.h"
#include "tokens.c"

typedef struct SemanticContext {
    ASTNode *return_type;
    ASTNode *expected_int_lit_type;
} SemanticContext;

static void report_error(int line, int column, const char *msg);

void semantic_check(ASTNode *program);

static void check_node(ASTNode *node, SemanticContext *ctx);

static void check_function_def(ASTNode *fn, SemanticContext *ctx);

static void check_decl(ASTNode *decl, SemanticContext *ctx);
static void check_return(ASTNode *ret, SemanticContext *ctx);
static void check_expr_stmt(ASTNode *expr_stmt, SemanticContext *ctx);
static void check_assign(ASTNode *assign, SemanticContext *ctx);

static ASTNode *check_expr(ASTNode *expr, SemanticContext *ctx);
static ASTNode *check_add_expr(ASTNode *add_expr, SemanticContext *ctx);
static ASTNode *check_mul_expr(ASTNode *mul_expr, SemanticContext *ctx);
static ASTNode *check_bitwise_expr(ASTNode *bitwise_expr, SemanticContext *ctx);
static ASTNode *check_unary_expr(ASTNode *unary_expr, SemanticContext *ctx);
static ASTNode *check_primary(ASTNode *primary, SemanticContext *ctx);
static ASTNode *check_call_expr(ASTNode *expr, SemanticContext *ctx);
