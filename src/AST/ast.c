#pragma once

#include "../file_stream.c"
#include "../int.c"
#include "ast_arena.c"

typedef enum {
    AST_PROGRAM,
    AST_FUNCTION_DEF,
    AST_PARAM_LIST,
    AST_PARAM,
    AST_BLOCK,
    AST_RETURN_STMT,
    AST_CONST_DECL,
    AST_MUT_DECL,
    AST_EXPR_STMT,
    AST_ASSIGN_STMT,
    AST_EXPRESSION,
    AST_ADD_EXPR,
    AST_CALL_EXPR,
    AST_ARG_LIST,
    AST_INT_LITERAL,
    AST_IDENTIFIER,
    AST_TYPE_NAME,
    // ── add new node kinds above this line ──
    AST_KIND_COUNT

} AstKind;

typedef struct ASTNode {
    AstKind kind;
    int line, column;
    struct ASTNode *parent;
    struct ASTNode *first_child;
    struct ASTNode *next_sibling;
    void *annotation;  // e.g. a semantic type pointer
    struct ASTNode *type;

    union {
        // AST_INT_LITERAL
        struct {
            int64_t i_val;
            uint64_t u_val;
            struct ASTNode *type;
        } int_lit;

        // AST_IDENTIFIER, AST_TYPE_NAME
        struct {
            char *name;
        } text;
    } data;
} ASTNode;

typedef void (*AstVisitor)(ASTNode *node, void *user_data);

// -----------------------------------------------------------------------------
// Core AST API
// -----------------------------------------------------------------------------
ASTNode *ast_new(AstKind kind, int line, int column) {
    ASTNode *n = ast_arena_alloc(sizeof(ASTNode));

    if (!n) {
        write(STDOUT_FILENO, "Error: Arena OOM\n", 17);
        return NULL;
    }
    n->kind = kind;
    n->line = line;
    n->column = column;
    n->parent = NULL;
    n->first_child = NULL;
    n->next_sibling = NULL;
    n->annotation = NULL;
    // Zero out payload union if desired:
    // memset(&n->data, 0, sizeof(n->data));
    return n;
}

void ast_add_child(ASTNode *parent, ASTNode *child) {
    if (!parent || !child) return;
    child->parent = parent;
    if (!parent->first_child) {
        parent->first_child = child;
    } else {
        ASTNode *c = parent->first_child;
        while (c->next_sibling) c = c->next_sibling;
        c->next_sibling = child;
    }
}

// simple depth-first pre-order traversal
void ast_traverse(ASTNode *root, AstVisitor visitor, void *user_data) {
    if (!root || !visitor) return;
    visitor(root, user_data);
    for (ASTNode *c = root->first_child; c; c = c->next_sibling) {
        ast_traverse(c, visitor, user_data);
    }
}
