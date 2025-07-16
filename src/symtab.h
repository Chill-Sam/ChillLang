// symtab.h
#pragma once

#include "AST/ast.c" // for ASTNode

// must be called once before parsing any files
void symtab_init(void);
void symtab_reset(void);

// ——— Type table ———

// return 1 if `name` is a known type, 0 otherwise
int symtab_is_type(const char *name);
// register a new type (built-in or user-defined)
void symtab_add_type(const char *name);
int symtab_same_type(const char *a, const char *b);

// ——— Function table ———

// register a new top-level function.  Errors on duplicate.
void symtab_add_function(ASTNode *fn_node);
// lookup a function by name.  Returns the ASTNode* or NULL.
ASTNode *symtab_find_function(const char *name);

// -- Variable Table (Scoped) --

void symtab_push_scope(void);
void symtab_pop_scope(void);

int symtab_add_var(const char *name, ASTNode *type_node, int is_mut);
ASTNode *symtab_find_var(const char *name);
int symtab_is_mut(const char *name);
