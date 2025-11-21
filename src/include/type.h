#pragma once

#include <stdbool.h>
#include <stdint.h>

struct AstNode;

typedef int16_t TypeId;

typedef enum TypeKind {
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_STRUCT,
} TypeKind;

typedef struct Type {
    TypeKind kind;

    int bit_width;
    int is_unsigned;

    struct AstNode *struct_decl;
} Type;

void types_init(void);
TypeId type_add(Type t);
const Type *type_get(TypeId id);

Type type_make_int(uint16_t bits, bool is_unsigned);
Type type_make_float(uint16_t bits);
Type type_make_void(void);
Type type_make_bool(void);
Type type_make_struct(struct AstNode *struct_decl);

extern TypeId TYPEID_INVALID;
extern TypeId TYPEID_VOID;
extern TypeId TYPEID_BOOL;
extern TypeId TYPEID_I8;
extern TypeId TYPEID_U8;
extern TypeId TYPEID_I16;
extern TypeId TYPEID_U16;
extern TypeId TYPEID_I32;
extern TypeId TYPEID_U32;
extern TypeId TYPEID_I64;
extern TypeId TYPEID_U64;
extern TypeId TYPEID_F32;
extern TypeId TYPEID_F64;

bool type_is_integer(TypeId id);
bool type_is_signed(TypeId id);
bool type_is_unsigned(TypeId id);
bool type_is_float(TypeId id);
uint16_t type_bit_width(TypeId id);
bool type_same_signedness(TypeId a, TypeId b);
TypeId type_binary_result(TypeId a, TypeId b);
bool type_can_implicitly_convert(TypeId src, TypeId dst);
