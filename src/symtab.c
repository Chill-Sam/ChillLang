#include "symtab.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *symtab_strdup(const char *str) {
    size_t len = strlen(str);
    char *copy = malloc(len + 1);
    if (!copy) {
        fprintf(stderr, "fatal: out of memory in symtab_strdup\n");
        abort();
    }

    memcpy(copy, str, len + 1);
    return copy;
}

static void scope_reserve(Scope *scope, uint32_t new_cap) {
    if (new_cap <= scope->capacity)
        return;

    Symbol **new_symbols = realloc(scope->symbols, new_cap * sizeof(Symbol *));
    if (!new_symbols) {
        fprintf(stderr, "fatal: out of memory in scope_reserve\n");
        abort();
    }

    scope->symbols  = new_symbols;
    scope->capacity = new_cap;
}

Scope *scope_create(Scope *parent) {
    Scope *s = malloc(sizeof *s);
    if (!s) {
        fprintf(stderr, "fatal: out of memory in scope_create\n");
        abort();
    }

    s->parent   = parent;
    s->symbols  = NULL;
    s->count    = 0;
    s->capacity = 0;
    return s;
}

void scope_destroy(Scope *scope) {
    if (!scope)
        return;

    for (uint32_t i = 0; i < scope->count; i++) {
        Symbol *sym = scope->symbols[i];
        if (sym) {
            // Free the owned name
            free((void *)sym->name);
            free(sym);
        }
    }

    free(scope->symbols);
    free(scope);
}

static Symbol *scope_lookup_array(Symbol **syms, uint32_t count,
                                  const char *name) {
    for (uint32_t i = 0; i < count; i++) {
        Symbol *sym = syms[i];
        if (sym && strcmp(sym->name, name) == 0) {
            return sym;
        }
    }
    return NULL;
}

Symbol *scope_lookup_local(Scope *scope, const char *name) {
    if (!scope)
        return NULL;
    return scope_lookup_array(scope->symbols, scope->count, name);
}

Symbol *scope_lookup(Scope *scope, const char *name) {
    for (Scope *s = scope; s != NULL; s = s->parent) {
        Symbol *sym = scope_lookup_local(s, name);
        if (sym)
            return sym;
    }
    return NULL;
}

static Symbol *scope_define(Scope *scope, const char *name, SymbolKind kind) {
    if (scope_lookup_local(scope, name)) {
        // NOTE: This is a duplicate definition error, caller should notify user
        return NULL;
    }

    Symbol *sym = malloc(sizeof *sym);
    if (!sym) {
        fprintf(stderr, "fatal: out of memory in scope_define\n");
        abort();
    }

    sym->kind = kind;
    sym->name = symtab_strdup(name);

    if (scope->count == scope->capacity) {
        uint32_t new_cap = scope->capacity ? (scope->capacity * 2) : 8;
        scope_reserve(scope, new_cap);
    }

    scope->symbols[scope->count++] = sym;
    return sym;
}

Symbol *scope_define_type(Scope *scope, const char *name, TypeId type_id) {
    Symbol *sym = scope_define(scope, name, SYM_TYPE);
    if (!sym)
        return NULL;

    sym->as.type.type_id = type_id;
    return sym;
}

Symbol *scope_define_func(Scope *scope, const char *name, AstNode *func_decl) {
    Symbol *sym = scope_define(scope, name, SYM_FUNC);
    if (!sym)
        return NULL;

    sym->as.func.func_decl = func_decl;
    return sym;
}

Symbol *scope_define_var(Scope *scope, const char *name, TypeId type_id,
                         bool is_mut, AstNode *decl_node) {
    Symbol *sym = scope_define(scope, name, SYM_VAR);
    if (!sym)
        return NULL;

    sym->as.var.type_id   = type_id;
    sym->as.var.is_mut    = is_mut;
    sym->as.var.decl_node = decl_node;
    return sym;
}
