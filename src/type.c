#include "type.h"
#include <stdio.h>
#include <stdlib.h>

#define TYPE_TABLE_INITIAL_CAP 32

static Type *type_table       = NULL;
static uint16_t type_count    = 0;
static uint16_t type_capacity = 0;

TypeId TYPEID_INVALID         = -1;
TypeId TYPEID_VOID            = 0;
TypeId TYPEID_BOOL            = 0;
TypeId TYPEID_I8              = 0;
TypeId TYPEID_U8              = 0;
TypeId TYPEID_I16             = 0;
TypeId TYPEID_U16             = 0;
TypeId TYPEID_I32             = 0;
TypeId TYPEID_U32             = 0;
TypeId TYPEID_I64             = 0;
TypeId TYPEID_U64             = 0;
TypeId TYPEID_F32             = 0;
TypeId TYPEID_F64             = 0;

static void type_table_reserve(uint16_t new_cap) {
    if (new_cap <= type_capacity)
        return;

    Type *new_table = realloc(type_table, new_cap * sizeof(Type));
    if (!new_table) {
        fprintf(stderr, "fatal: out of memory in type_table_reserve\n");
        abort();
    }

    type_table    = new_table;
    type_capacity = new_cap;
}

Type type_make_void(void) {
    return (Type){.kind        = TYPE_VOID,
                  .bit_width   = 0,
                  .is_unsigned = false,
                  .struct_decl = NULL};
}

Type type_make_bool(void) {
    return (Type){.kind        = TYPE_BOOL,
                  .bit_width   = 1,
                  .is_unsigned = true,
                  .struct_decl = NULL};
}

Type type_make_int(uint16_t bits, bool is_unsigned) {
    return (Type){.kind        = TYPE_INT,
                  .bit_width   = bits,
                  .is_unsigned = is_unsigned,
                  .struct_decl = NULL};
}

Type type_make_float(uint16_t bits) {
    return (Type){.kind        = TYPE_FLOAT,
                  .bit_width   = bits,
                  .is_unsigned = false,
                  .struct_decl = NULL};
}

Type type_make_struct(struct AstNode *struct_decl) {
    return (Type){.kind        = TYPE_STRUCT,
                  .bit_width   = 0,
                  .is_unsigned = false,
                  .struct_decl = struct_decl};
}

TypeId type_add(Type t) {
    if (type_count == type_capacity) {
        uint16_t new_cap = type_capacity ? (uint16_t)(type_capacity * 2)
                                         : TYPE_TABLE_INITIAL_CAP;
        if (new_cap <= type_capacity) {
            fprintf(stderr, "fatal: type table capacity overflow\n");
            abort();
        }

        type_table_reserve(new_cap);
    }

    TypeId id      = type_count;
    type_table[id] = t;
    type_count++;
    return id;
}

const Type *type_get(TypeId id) {
    if (id >= type_count) {
        fprintf(stderr, "fatal: invalid type id %u\n", id);
        abort();
    }
    return &type_table[id];
}

void types_init(void) {
    TYPEID_VOID = type_add(type_make_void());
    TYPEID_BOOL = type_add(type_make_bool());

    TYPEID_I8   = type_add(type_make_int(8, false));
    TYPEID_U8   = type_add(type_make_int(8, true));
    TYPEID_I16  = type_add(type_make_int(16, false));
    TYPEID_U16  = type_add(type_make_int(16, true));
    TYPEID_I32  = type_add(type_make_int(32, false));
    TYPEID_U32  = type_add(type_make_int(32, true));
    TYPEID_I64  = type_add(type_make_int(64, false));
    TYPEID_U64  = type_add(type_make_int(64, true));

    TYPEID_F32  = type_add(type_make_float(32));
    TYPEID_F64  = type_add(type_make_float(64));
}

bool type_is_integer(TypeId id) {
    const Type *t = type_get(id);
    return t->kind == TYPE_INT;
}

bool type_is_float(TypeId id) {
    const Type *t = type_get(id);
    return t->kind == TYPE_FLOAT;
}

bool type_is_signed(TypeId id) {
    const Type *t = type_get(id);
    return t->kind == TYPE_INT && !t->is_unsigned;
}

bool type_is_unsigned(TypeId id) {
    const Type *t = type_get(id);
    return t->kind == TYPE_INT && t->is_unsigned;
}

uint16_t type_bit_width(TypeId id) {
    const Type *t = type_get(id);
    return t->bit_width;
}

bool type_same_signedness(TypeId a, TypeId b) {
    return type_is_signed(a) == type_is_signed(b);
}

TypeId type_binary_result(TypeId a, TypeId b) {
    if (!type_is_integer(a) || !type_is_integer(b))
        return TYPEID_INVALID;

    if (!type_same_signedness(a, b))
        return TYPEID_INVALID;

    int wa = type_bit_width(a);
    int wb = type_bit_width(b);

    // TODO: Handle floats
    return (wa >= wb) ? a : b;
}

bool type_can_implicitly_convert(TypeId src, TypeId dst) {
    if (!type_is_integer(src) || !type_is_integer(dst))
        return false;

    if (!type_same_signedness(src, dst))
        return false;

    // TODO: Handle floats
    return type_bit_width(src) <= type_bit_width(dst);
}
