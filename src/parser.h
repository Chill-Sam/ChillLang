#pragma once

#include "AST/ast.c"

ASTNode *parse_program(void);
ASTNode *parse_function_def(void);
ASTNode *parse_block(void);
ASTNode *parse_statement(void);
ASTNode *parse_const_decl(void);
ASTNode *parse_mut_decl(void);
ASTNode *parse_return_stmt(void);
ASTNode *parse_expr_stmt(void);
ASTNode *parse_assign_stmt(void);
ASTNode *parse_expression(void);
ASTNode *parse_add_expr(void);
ASTNode *parse_mul_expr(void);
ASTNode *parse_primary(void);
ASTNode *parse_call_expr(void);
ASTNode *parse_arg_list(void);
