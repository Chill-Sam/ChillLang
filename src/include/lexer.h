#pragma once

#include "token.h"
#include <stddef.h>
#include <stdio.h>

typedef struct Lexer Lexer;

typedef void (*LexerDiagFn)(void *user_data, uint32_t line, uint32_t column,
                            const char *msg);

typedef struct LexerConfig {
    LexerDiagFn diag_fn;
    void *diag_user;
} LexerConfig;

Lexer *lexer_create(const char *src, size_t length, LexerConfig cfg);
void lexer_destroy(Lexer *lx);

Token lexer_next(Lexer *lx);
Token lexer_peek(Lexer *lx);
void lexer_consume(Lexer *lx);

void lexer_dump(Lexer *lx);
