#pragma once
#include "token.h"
#include "type.h"
#include <stdbool.h>
#include <stdint.h>

typedef enum AstNodeKind {
    AST_TRANSLATION_UNIT,
    AST_FUNC_DECL,
    AST_PARAM,
    AST_STRUCT_DECL,
    AST_FIELD,

    AST_BLOCK_STMT,
    AST_RETURN_STMT,
    AST_EXPR_STMT,
    AST_VAR_DECL,
    AST_IF_STMT,
    AST_WHILE_STMT,
    AST_FOR_STMT,
    AST_BREAK_STMT,
    AST_CONTINUE_STMT,

    AST_BIN_EXPR,
    AST_UNARY_EXPR,
    AST_CALL_EXPR,
    AST_MEMBER_EXPR,
    AST_ASSIGN_EXPR,
    AST_CAST_EXPR,

    AST_IDENT_EXPR,
    AST_LITERAL_EXPR,
} AstNodeKind;

typedef struct AstNode AstNode;

typedef struct AstNodeList {
    AstNode **items;
    uint32_t count;
    uint32_t capacity;
} AstNodeList;

typedef struct AstTranslationUnit {
    AstNodeList items; // funcs + structs
} AstTranslationUnit;

typedef struct AstParam {
    AstNode *type; // for now: IDENT_EXPR
    bool is_mut;
    Token name;
} AstParam;

typedef struct AstFuncDecl {
    Token name;
    AstNodeList params;   // AST_PARAM nodes
    AstNode *return_type; // TODO: parse later, NULL = void
    AstNode *body;        // AST_BLOCK_STMT
} AstFuncDecl;

typedef struct AstField {
    AstNode *type; // IDENT_EXPR
    // bool is_mut;
    Token name;
} AstField;

typedef struct AstStructDecl {
    Token name;
    AstNodeList fields; // AST_FIELD nodes
} AstStructDecl;

typedef struct AstBlockStmt {
    AstNodeList stmts; // list of statements
} AstBlockStmt;

typedef struct AstReturnStmt {
    AstNode *expr; // NULL for bare return;
} AstReturnStmt;

typedef struct AstExprStmt {
    AstNode *expr;
} AstExprStmt;

typedef struct AstVarDecl {
    AstNode *type;
    bool is_mut;
    Token name;
    AstNode *init; // NULL for no init
} AstVarDecl;

typedef struct AstIfStmt {
    AstNode *cond;
    AstNode *then_block;
    AstNode *else_block;
} AstIfStmt;

typedef struct AstWhileStmt {
    AstNode *cond;
    AstNode *body;
} AstWhileStmt;

typedef struct AstForStmt {
    AstNode *init;
    AstNode *cond;
    AstNode *post;
    AstNode *body;
} AstForStmt;

typedef enum AstBinOp {
    BIN_ADD,
    BIN_SUB,
    BIN_MUL,
    BIN_DIV,
    BIN_MOD,
    BIN_LT,
    BIN_GT,
    BIN_LE,
    BIN_GE,
    BIN_EQ,
    BIN_NE,
    BIN_BIT_AND,
    BIN_BIT_XOR,
    BIN_BIT_OR,
    BIN_SHL,
    BIN_SHR,
    BIN_AND,
    BIN_OR,
} AstBinOp;

typedef enum AstUnaryOp {
    UN_NEG,    // -x
    UN_NOT,    // !x
    UN_BITNOT, // ~x
} AstUnaryOp;

typedef enum AstAssignOp {
    ASSIGN_EQ,     // =
    ASSIGN_ADD_EQ, // +=
    ASSIGN_SUB_EQ, // -=
    ASSIGN_MUL_EQ, // *=
    ASSIGN_DIV_EQ, // /=
    ASSIGN_MOD_EQ, // %=
} AstAssignOp;

typedef struct AstBinExpr {
    AstBinOp op;
    AstNode *lhs;
    AstNode *rhs;
} AstBinExpr;

typedef struct AstUnaryExpr {
    AstUnaryOp op;
    AstNode *expr;
} AstUnaryExpr;

typedef struct AstAssignExpr {
    AstAssignOp op;
    AstNode *lhs;
    AstNode *rhs;
} AstAssignExpr;

typedef struct AstCastExpr {
    AstNode *expr;
    AstNode *target;
    TypeId target_type;
} AstCastExpr;

typedef struct AstCallExpr {
    AstNode *callee; // IDENT_EXPR usually
    AstNodeList args;
} AstCallExpr;

typedef struct AstIdentExpr {
    Token name;
} AstIdentExpr;

typedef struct AstLiteralExpr {
    Token tok; // int/float/string/char
} AstLiteralExpr;

typedef struct AstMemberExpr {
    AstNode *base;
    Token field;
} AstMemberExpr;

struct AstNode {
    AstNodeKind kind;
    TypeId type_id;

    union {
        AstTranslationUnit translation_unit;
        AstFuncDecl func_decl;
        AstParam param;
        AstStructDecl struct_decl;
        AstField field;
        AstBlockStmt block_stmt;
        AstReturnStmt return_stmt;
        AstExprStmt expr_stmt;
        AstVarDecl var_decl;
        AstIfStmt if_stmt;
        AstWhileStmt while_stmt;
        AstForStmt for_stmt;

        AstBinExpr bin_expr;
        AstUnaryExpr unary_expr;
        AstCallExpr call_expr;
        AstMemberExpr member_expr;
        AstAssignExpr assign_expr;
        AstCastExpr cast_expr;

        AstIdentExpr ident_expr;
        AstLiteralExpr literal_expr;
    } as;
};
void ast_list_init(AstNodeList *list);
void ast_list_push(AstNodeList *list, AstNode *node);
void ast_list_free(AstNodeList *list);

void ast_dump(AstNode *node);
