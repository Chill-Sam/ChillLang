#include "ir_type.h"

#include "AST/ast_arena.c"

Type *types[IR_MAX_TYPES];
size_t n_types = 0;

Type *get_type(const char *name) {
    for (size_t i = 0; i < n_types; i++) {
        const char *a = types[i]->name;
        const char *b = name;
        int j         = 0;
        while (a[j] && b[j] && a[j] == b[j])
            j++;
        if (a[j] == '\0' && b[j] == '\0') {
            return types[i];
        }
    }

    return NULL;
}

int add_type(Type *ty) {
    if (n_types >= IR_MAX_TYPES)
        return 0;
    if (get_type(ty->name))
        return 0;
    types[n_types++] = ty;
    return 1;
}

void init_builtin_types(void) {
    Type *t;

    // i8
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_INT;
    t->size = 1;
    t->name = ast_strdup("i8", 2);
    add_type(t);

    // i16
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_INT;
    t->size = 2;
    t->name = ast_strdup("i16", 3);
    add_type(t);

    // i32
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_INT;
    t->size = 4;
    t->name = ast_strdup("i32", 3);
    add_type(t);

    // i64
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_INT;
    t->size = 8;
    t->name = ast_strdup("i64", 3);
    add_type(t);

    // u8
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_UINT;
    t->size = 1;
    t->name = ast_strdup("u8", 2);
    add_type(t);

    // u16
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_UINT;
    t->size = 2;
    t->name = ast_strdup("u16", 3);
    add_type(t);

    // u32
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_UINT;
    t->size = 4;
    t->name = ast_strdup("u32", 3);
    add_type(t);

    // u64
    t       = ast_arena_alloc(sizeof *t);
    t->kind = TY_UINT;
    t->size = 8;
    t->name = ast_strdup("u64", 3);
    add_type(t);
}
