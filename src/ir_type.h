#pragma once

#include "int.c"
#define IR_MAX_TYPES 128

typedef enum {
    TY_INT,
    TY_UINT,
} TypeKind;

typedef struct Type {
    char *name;
    TypeKind kind;
    size_t size;
} Type;

Type *get_type(const char *name);
int add_type(Type *ty);
void init_builtin_types(void);
