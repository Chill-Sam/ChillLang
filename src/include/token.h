#pragma once

#include <stdint.h>

typedef enum IntSuffixKind {
    INT_SUFFIX_NONE = 0,
    INT_SUFFIX_I8,
    INT_SUFFIX_I16,
    INT_SUFFIX_I32,
    INT_SUFFIX_I64,
    INT_SUFFIX_U8,
    INT_SUFFIX_U16,
    INT_SUFFIX_U32,
    INT_SUFFIX_U64,
} IntSuffixKind;

typedef struct IntLiteralInfo {
    uint8_t base;
    IntSuffixKind suffix;
} IntLiteralInfo;

typedef enum FloatSuffixKind {
    FLOAT_SUFFIX_NONE = 0,
    FLOAT_SUFFIX_F32,
    FLOAT_SUFFIX_F64,
} FloatSuffixKind;

typedef struct FloatLiteralInfo {
    FloatSuffixKind suffix;
} FloatLiteralInfo;

typedef enum TokenKind {
    TOK_INVALID = -1,
    TOK_EOF     = 0,

    TOK_IDENT,

    TOK_INT_LITERAL,
    TOK_FLOAT_LITERAL,
    TOK_CHAR_LITERAL,
    TOK_STRING_LITERAL,

    // keywords
    TOK_KW_IF,
    TOK_KW_ELSE,
    TOK_KW_WHILE,
    TOK_KW_FOR,
    TOK_KW_RETURN,
    TOK_KW_STRUCT,
    TOK_KW_FUN,
    TOK_KW_MUT,
    TOK_KW_AS,
    TOK_KW_AND,
    TOK_KW_OR,
    TOK_KW_NOT,
    TOK_KW_TRUE,
    TOK_KW_FALSE,
    TOK_KW_BREAK,
    TOK_KW_CONTINUE,

    // operators / punctuation
    TOK_PLUS,    // +
    TOK_MINUS,   // -
    TOK_STAR,    // *
    TOK_SLASH,   // /
    TOK_PERCENT, // %
    TOK_AMP,     // &
    TOK_PIPE,    // |
    TOK_CARET,   // ^
    TOK_TILDE,   // ~
    TOK_BANG,    // !
    TOK_EQ,      // =
    TOK_LT,      // <
    TOK_GT,      // >

    TOK_SHL, // <<
    TOK_SHR, // >>

    TOK_PLUS_EQ,    // +=
    TOK_MINUS_EQ,   // -=
    TOK_STAR_EQ,    // *=
    TOK_SLASH_EQ,   // /=
    TOK_PERCENT_EQ, // %=
    TOK_AMP_EQ,     // &=
    TOK_PIPE_EQ,    // |=
    TOK_CARET_EQ,   // ^=

    TOK_EQ_EQ,     // ==
    TOK_BANG_EQ,   // !=
    TOK_LT_EQ,     // <=
    TOK_GT_EQ,     // >=
    TOK_AMP_AMP,   // &&
    TOK_PIPE_PIPE, // ||

    TOK_LPAREN,   // (
    TOK_RPAREN,   // )
    TOK_LBRACE,   // {
    TOK_RBRACE,   // }
    TOK_LBRACKET, // [
    TOK_RBRACKET, // ]
    TOK_COMMA,    // ,
    TOK_SEMI,     // ;
    TOK_COLON,    // :
    TOK_DOT,      // .
    TOK_ARROW,    // ->
} TokenKind;

typedef struct Token {
    TokenKind kind;

    uint32_t offset;
    uint32_t length;

    uint32_t line;
    uint32_t column;

    const char *lexeme; // points into the original source buffer

    union {
        IntLiteralInfo int_literal;
        FloatLiteralInfo float_literal;
    } lit;
} Token;

static const char *token_kind_name(TokenKind k) {
    switch (k) {
    case TOK_EOF:
        return "EOF";
    case TOK_INVALID:
        return "INVALID";
    case TOK_IDENT:
        return "IDENT";

    case TOK_INT_LITERAL:
        return "INT_LITERAL";
    case TOK_FLOAT_LITERAL:
        return "FLOAT_LITERAL";
    case TOK_CHAR_LITERAL:
        return "CHAR_LITERAL";
    case TOK_STRING_LITERAL:
        return "STRING_LITERAL";

    case TOK_KW_IF:
        return "KW_IF";
    case TOK_KW_ELSE:
        return "KW_ELSE";
    case TOK_KW_WHILE:
        return "KW_WHILE";
    case TOK_KW_FOR:
        return "KW_FOR";
    case TOK_KW_RETURN:
        return "KW_RETURN";
    case TOK_KW_STRUCT:
        return "KW_STRUCT";
    case TOK_KW_FUN:
        return "KW_FUN";
    case TOK_KW_MUT:
        return "KW_MUT";
    case TOK_KW_AS:
        return "KW_AS";
    case TOK_KW_AND:
        return "KW_AND";
    case TOK_KW_OR:
        return "KW_OR";
    case TOK_KW_NOT:
        return "KW_NOT";

    case TOK_PLUS:
        return "PLUS";
    case TOK_MINUS:
        return "MINUS";
    case TOK_STAR:
        return "STAR";
    case TOK_SLASH:
        return "SLASH";
    case TOK_PERCENT:
        return "PERCENT";
    case TOK_AMP:
        return "AMP";
    case TOK_PIPE:
        return "PIPE";
    case TOK_CARET:
        return "CARET";
    case TOK_TILDE:
        return "TILDE";
    case TOK_BANG:
        return "BANG";
    case TOK_EQ:
        return "EQ";
    case TOK_LT:
        return "LT";
    case TOK_GT:
        return "GT";

    case TOK_PLUS_EQ:
        return "PLUS_EQ";
    case TOK_MINUS_EQ:
        return "MINUS_EQ";
    case TOK_STAR_EQ:
        return "STAR_EQ";
    case TOK_SLASH_EQ:
        return "SLASH_EQ";
    case TOK_PERCENT_EQ:
        return "PERCENT_EQ";
    case TOK_AMP_EQ:
        return "AMP_EQ";
    case TOK_PIPE_EQ:
        return "PIPE_EQ";
    case TOK_CARET_EQ:
        return "CARET_EQ";

    case TOK_EQ_EQ:
        return "EQ_EQ";
    case TOK_BANG_EQ:
        return "BANG_EQ";
    case TOK_LT_EQ:
        return "LT_EQ";
    case TOK_GT_EQ:
        return "GT_EQ";
    case TOK_AMP_AMP:
        return "AMP_AMP";
    case TOK_PIPE_PIPE:
        return "PIPE_PIPE";

    case TOK_LPAREN:
        return "LPAREN";
    case TOK_RPAREN:
        return "RPAREN";
    case TOK_LBRACE:
        return "LBRACE";
    case TOK_RBRACE:
        return "RBRACE";
    case TOK_LBRACKET:
        return "LBRACKET";
    case TOK_RBRACKET:
        return "RBRACKET";
    case TOK_COMMA:
        return "COMMA";
    case TOK_SEMI:
        return "SEMI";
    case TOK_COLON:
        return "COLON";
    case TOK_DOT:
        return "DOT";
    case TOK_ARROW:
        return "ARROW";

    default:
        return "UNKNOWN";
    }
}

static const char *int_suffix_name(IntSuffixKind sk) {
    switch (sk) {
    case INT_SUFFIX_NONE:
        return "none";
    case INT_SUFFIX_I8:
        return "i8";
    case INT_SUFFIX_I16:
        return "i16";
    case INT_SUFFIX_I32:
        return "i32";
    case INT_SUFFIX_I64:
        return "i64";
    case INT_SUFFIX_U8:
        return "u8";
    case INT_SUFFIX_U16:
        return "u16";
    case INT_SUFFIX_U32:
        return "u32";
    case INT_SUFFIX_U64:
        return "u64";
    default:
        return "<?>"; // defensive
    }
}

static const char *float_suffix_name(FloatSuffixKind sk) {
    switch (sk) {
    case FLOAT_SUFFIX_NONE:
        return "none";
    case FLOAT_SUFFIX_F32:
        return "f32";
    case FLOAT_SUFFIX_F64:
        return "f64";
    default:
        return "<?>"; // defensive
    }
}
