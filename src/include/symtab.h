#pragma once

#include "ast.h"
#include "type.h"

typedef enum SymbolKind {
    // Global symbols
    SYM_TYPE,
    SYM_FUNC,

    // Scoped symbols
    SYM_VAR,
} SymbolKind;

typedef struct Symbol {
    SymbolKind kind;
    const char *name;

    union {
        struct {
            TypeId type_id;
        } type;

        struct {
            AstNode *func_decl;
        } func;

        struct {
            TypeId type_id;
            bool is_mut;
            AstNode *decl_node;
        } var;
    } as;
} Symbol;

typedef struct Scope {
    struct Scope *parent;

    Symbol **symbols;
    uint32_t count;
    uint32_t capacity;
} Scope;

// Scope management
Scope *scope_create(Scope *parent);
void scope_destroy(Scope *scope);

// Lookup
Symbol *scope_lookup(Scope *scope, const char *name);
Symbol *scope_lookup_local(Scope *scope, const char *name);

// Definition helpers
Symbol *scope_define_type(Scope *scope, const char *name, TypeId type_id);
Symbol *scope_define_func(Scope *scope, const char *name, AstNode *func_decl);
Symbol *scope_define_var(Scope *scope, const char *name, TypeId type_id,
                         bool is_mut, AstNode *decl_node);
