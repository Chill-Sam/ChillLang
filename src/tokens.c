#pragma once

#include "AST/ast_log.c"
#include "file_stream.c"
#include "int.c"

#define MAX_LEXEME 64
#define MAX_ERR_MSG 64

int lex_errors = 0;

enum TokenType {
    // Symbols
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_SEMI,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_COMMA,
    TOKEN_EQUALS,  // 8
    TOKEN_SHIFT_LEFT,
    TOKEN_SHIFT_RIGHT,
    TOKEN_AMPERSAND,
    TOKEN_PIPE,
    TOKEN_XOR,
    TOKEN_NOT,
    TOKEN_LOGICAL_EQUALS,
    TOKEN_LOGICAL_NOT_EQUALS,  // 16
    TOKEN_LOGICAL_LESS,
    TOKEN_LOGICAL_GREATER,
    TOKEN_LOGICAL_LESS_EQUALS,
    TOKEN_LOGICAL_GREATER_EQUALS,
    TOKEN_LOGICAL_AND,
    TOKEN_LOGICAL_OR,

    // Literals
    TOKEN_I8_LITERAL,
    TOKEN_I16_LITERAL,
    TOKEN_I32_LITERAL,
    TOKEN_I64_LITERAL,  // 24
    TOKEN_INT_LITERAL,
    TOKEN_U8_LITERAL,
    TOKEN_U16_LITERAL,
    TOKEN_U32_LITERAL,
    TOKEN_U64_LITERAL,

    // Builtin types
    // TOKEN_U8,
    // TOKEN_U16,
    // TOKEN_U32,
    // TOKEN_U64,
    // TOKEN_I8,
    // TOKEN_I16,
    // TOKEN_I32,
    // TOKEN_I64,

    // Keywords
    TOKEN_IDENTIFIER,
    TOKEN_RETURN,
    TOKEN_MUT,

    // Misc
    TOKEN_ERROR,
    TOKEN_EOF,
};

typedef struct {
    enum TokenType type;
    int line, column;
    int length;
    char lexeme[MAX_LEXEME + 1];

    union {
        struct {
            int64_t i;
            uint64_t u;
        } literal;

        struct {
            char message[MAX_ERR_MSG + 1];
        } error;
    } data;
} Token;

Token make_error_token(int line, int column, const char *text, int text_len,
                       const char *err_msg) {
    lex_errors++;
    Token tok;
    tok.type = TOKEN_ERROR;

    tok.line = line;
    tok.column = column;

    // Copy the offending text into tok.lexeme
    tok.length = text_len < (int)sizeof(tok.lexeme)
                     ? text_len
                     : (int)sizeof(tok.lexeme) - 1;
    for (int i = 0; i < tok.length; i++) {
        tok.lexeme[i] = text[i];
    }
    tok.lexeme[tok.length] = '\0';

    // Copy the error message
    int msg_len = 0;
    while (err_msg[msg_len] != '\0' && msg_len < MAX_ERR_MSG) {
        tok.data.error.message[msg_len] = err_msg[msg_len];
        msg_len++;
    }
    tok.data.error.message[msg_len] = '\0';

    return tok;
}

void output_token(Token tok) {
    write(STDOUT_FILENO, tok.lexeme, tok.length);
    write(STDOUT_FILENO, ": lexeme\n", 9);
    char numbuf[12];
    int numlen = itoa_int((int)tok.type, numbuf);
    write(STDOUT_FILENO, numbuf, numlen);
    write(STDOUT_FILENO, ": tokentype\n", 12);

    switch (tok.type) {
        case TOKEN_I8_LITERAL:
        case TOKEN_I16_LITERAL:
        case TOKEN_I32_LITERAL:
        case TOKEN_I64_LITERAL: {
            char num[64];
            int n = itoa_int(tok.data.literal.i, num);
            write(STDOUT_FILENO, num, n);
            write(STDOUT_FILENO, ": value\n", 8);
            break;
        }

        case TOKEN_U8_LITERAL:
        case TOKEN_U16_LITERAL:
        case TOKEN_U32_LITERAL:
        case TOKEN_U64_LITERAL: {
            char num[64];
            int n = itoa_int(tok.data.literal.u, num);
            write(STDOUT_FILENO, num, n);
            write(STDOUT_FILENO, ": value\n", 8);
            break;
        }

        default:
            // not a literal type; nothing to do
            break;
    }

    if (tok.type == TOKEN_ERROR) {
        write(STDOUT_FILENO, tok.data.error.message,
              str_len(tok.data.error.message));
    }

    write(STDOUT_FILENO, "\n", 1);
}
