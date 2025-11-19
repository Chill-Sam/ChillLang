#pragma once
#include "ast.h"
#include "lexer.h"

typedef struct Parser {
    Lexer *lx;
    Token cur;
    Token peek;
    int has_peek;
} Parser;

void parser_init(Parser *p, Lexer *lx);
AstNode *parse_translation_unit(Parser *p);
void free_translation_unit(AstNode *tu);

void parser_dump(Parser *p);
