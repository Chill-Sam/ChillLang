#include "lexer.h"
#include "token.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

struct Lexer {
    const char *src;
    size_t length;

    const char *cur;
    const char *end;

    uint32_t line;
    uint32_t column;

    LexerConfig cfg;

    Token lookahead;
    int has_lookahead;
};

static void report(Lexer *lx, const char *msg) {
    if (lx->cfg.diag_fn) {
        lx->cfg.diag_fn(lx->cfg.diag_user, lx->line, lx->column, msg);
    }
}

// NOTE: IMPORTANT: NEEDS TO BE MANUALLY FREED
Lexer *lexer_create(const char *src, size_t length, LexerConfig cfg) {
    Lexer *lx = malloc(sizeof *lx);
    if (!lx)
        return NULL;

    lx->src           = src;
    lx->length        = length;
    lx->cur           = src;
    lx->end           = src + length;
    lx->line          = 1;
    lx->column        = 1;
    lx->cfg           = cfg;
    lx->has_lookahead = 0;

    return lx;
}

void lexer_destroy(Lexer *lx) { free(lx); }

static int at_end(Lexer *lx) { return lx->cur >= lx->end; }

static char peek_char(Lexer *lx) {
    if (at_end(lx))
        return '\0';
    return *lx->cur;
}

static char peek_next_char(Lexer *lx) {
    if (at_end(lx))
        return '\0';
    return lx->cur[1];
}

static char advance_char(Lexer *lx) {
    if (at_end(lx))
        return '\0';
    char c = *lx->cur++;
    if (c == '\n') {
        lx->line++;
        lx->column = 1;
    } else {
        lx->column++;
    }
    return c;
}

static void skip_whitespace_and_comments(Lexer *lx) {
    for (;;) {
        char c = peek_char(lx);
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance_char(lx);
            continue;
        }

        if (c == '/' && peek_next_char(lx) == '/') {
            while (peek_char(lx) != '\n' && !at_end(lx))
                advance_char(lx);
            continue;
        }

        if (c == '/' && peek_next_char(lx) == '*') {
            advance_char(lx);
            advance_char(lx);
            int closed = 0;
            while (!at_end(lx)) {
                if (peek_char(lx) == '*' && peek_next_char(lx) == '/') {
                    advance_char(lx);
                    advance_char(lx);
                    closed = 1;
                    break;
                }
                advance_char(lx);
            }
            if (!closed)
                report(lx, "Unterminated comment");
            continue;
        }

        break;
    }
}

typedef struct {
    const char *name;
    TokenKind kind;
} Keyword;

static const Keyword keywords[] = {
    {"if", TOK_KW_IF},         {"else", TOK_KW_ELSE},
    {"while", TOK_KW_WHILE},   {"for", TOK_KW_FOR},
    {"return", TOK_KW_RETURN}, {"struct", TOK_KW_STRUCT},
    {"fun", TOK_KW_FUN},       {"mut", TOK_KW_MUT},
    {"as", TOK_KW_AS},         {"and", TOK_KW_AND},
    {"or", TOK_KW_OR},         {"not", TOK_KW_NOT},
    {"true", TOK_KW_TRUE},     {"false", TOK_KW_FALSE},
    {"break", TOK_KW_BREAK},   {"continue", TOK_KW_CONTINUE}};

static TokenKind lookup_keyword(const char *start, size_t len) {
    for (size_t i = 0; i < sizeof keywords / sizeof *keywords; i++) {
        const Keyword *kw = &keywords[i];
        if (strlen(kw->name) == len && memcmp(kw->name, start, len) == 0)
            return kw->kind;
    }
    return TOK_IDENT;
}

static Token make_token(Lexer *lx, TokenKind kind, const char *start) {
    Token t;
    t.kind   = kind;
    t.offset = (uint32_t)(start - lx->src);
    t.length = (uint32_t)(lx->cur - start);
    t.line   = lx->line;
    t.column = lx->column - t.length;
    t.lexeme = start;
    return t;
}

static int is_digit_base(char c, int base) {
    switch (base) {
    case 2:
        return c == '0' || c == '1';
    case 8:
        return c >= '0' && c <= '7';
    case 10:
        return c >= '0' && c <= '9';
    case 16:
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
               (c >= 'A' && c <= 'F');
    default:
        return 0;
    }
}

static IntSuffixKind make_suffix_kind(int is_unsigned, int width) {
    if (is_unsigned) {
        switch (width) {
        case 8:
            return INT_SUFFIX_U8;
        case 16:
            return INT_SUFFIX_U16;
        case 32:
            return INT_SUFFIX_U32;
        case 64:
            return INT_SUFFIX_U64;
        default:
            return INT_SUFFIX_NONE;
        }
    } else {
        switch (width) {
        case 8:
            return INT_SUFFIX_I8;
        case 16:
            return INT_SUFFIX_I16;
        case 32:
            return INT_SUFFIX_I32;
        case 64:
            return INT_SUFFIX_I64;
        default:
            return INT_SUFFIX_NONE;
        }
    }
}

static Token lex_identifier_or_keyword(Lexer *lx, const char *start,
                                       uint32_t line, uint32_t col) {
    while (!at_end(lx) &&
           (isalnum((unsigned char)peek_char(lx)) || peek_char(lx) == '_')) {
        advance_char(lx);
    }

    Token t;
    t.offset       = (uint32_t)(start - lx->src);
    t.length       = (uint32_t)(lx->cur - start);
    t.line         = line;
    t.column       = col;
    t.lexeme       = start;

    TokenKind kind = lookup_keyword(start, t.length);
    t.kind         = kind;
    return t;
}

static Token lex_number(Lexer *lx, const char *start, uint32_t line,
                        uint32_t col) {
    int is_float            = 0;
    int base                = 10;

    const char *digit_start = start;

    if (*start == '0') {
        char p = peek_char(lx);
        if (p == 'x' || p == 'X' || p == 'b' || p == 'B' || p == 'o' ||
            p == 'O') {
            advance_char(lx);
            switch (p) {
            case 'x':
            case 'X':
                base = 16;
                break;
            case 'b':
            case 'B':
                base = 2;
                break;
            case 'o':
            case 'O':
                base = 8;
                break;
            default:
                base = 10;
                break;
            }
            digit_start = lx->cur;
        }
    }

    while (!at_end(lx) && is_digit_base(peek_char(lx), base)) {
        advance_char(lx);
    }

    if (lx->cur == digit_start) {
        report(lx, "expected digits after base prefix");
    }

    if (base == 10 && !at_end(lx) && peek_char(lx) == '.' &&
        isdigit((unsigned char)peek_next_char(lx))) {
        is_float = 1;
        advance_char(lx);
        while (!at_end(lx) && isdigit((unsigned char)peek_char(lx))) {
            advance_char(lx);
        }
    }

    FloatLiteralInfo float_info;
    float_info.suffix = FLOAT_SUFFIX_NONE;

    IntLiteralInfo int_info;
    int_info.base   = (uint8_t)base;
    int_info.suffix = INT_SUFFIX_NONE;

    if (is_float) {
        float_info.suffix = FLOAT_SUFFIX_NONE;

        char c            = peek_char(lx);
        if (c == 'f' || c == 'F') {
            advance_char(lx);
            if (at_end(lx) || !isdigit((unsigned char)peek_char(lx))) {
                report(lx, "missing digits after float suffix");
            } else {
                int width = 0;
                while (!at_end(lx) && isdigit((unsigned char)peek_char(lx))) {
                    char d = advance_char(lx);
                    // (d - '0') converts digit char to integer representation
                    width = width * 10 + (d - '0');
                }

                FloatSuffixKind suffix = FLOAT_SUFFIX_NONE;
                if (width == 32) {
                    suffix = FLOAT_SUFFIX_F32;
                } else if (width == 64) {
                    suffix = FLOAT_SUFFIX_F64;
                }
                if (suffix == FLOAT_SUFFIX_NONE) {
                    report(lx, "invalid float width");
                } else {
                    float_info.suffix = suffix;
                }
            }
        }
    } else {
        char c = peek_char(lx);
        if (c == 'u' || c == 'U' || c == 'i' || c == 'I') {
            advance_char(lx);
            int is_unsigned = c == 'u' || c == 'U';

            if (at_end(lx) || !isdigit((unsigned char)peek_char(lx))) {
                report(lx, "missing digits after integer suffix");
            } else {
                int width = 0;
                while (!at_end(lx) && isdigit((unsigned char)peek_char(lx))) {
                    char d = advance_char(lx);
                    // (d - '0') converts digit char to integer representation
                    width = width * 10 + (d - '0');
                }

                IntSuffixKind suffix = make_suffix_kind(is_unsigned, width);
                if (suffix == INT_SUFFIX_NONE) {
                    report(lx, "invalid integer width");
                }

                int_info.suffix = suffix;
            }
        }
    }

    Token t;
    t.offset = (uint32_t)(start - lx->src);
    t.length = (uint32_t)(lx->cur - start);
    t.line   = line;
    t.column = col;
    t.lexeme = start;

    t.kind   = is_float ? TOK_FLOAT_LITERAL : TOK_INT_LITERAL;
    if (is_float) {
        t.lit.float_literal = float_info;
    } else {
        t.lit.int_literal = int_info;
    }

    return t;
}

static Token lex_string(Lexer *lx, char quote, const char *start, uint32_t line,
                        uint32_t col) {
    int is_char = (quote == '\'');

    while (!at_end(lx) && peek_char(lx) != quote) {
        char c = advance_char(lx);
        if (c == '\\' && !at_end(lx)) {
            advance_char(lx);
        }
    }

    if (at_end(lx)) {
        report(lx, "unterminated string literal");
    } else {
        advance_char(lx);
    }

    Token t;
    t.offset = (uint32_t)(start - lx->src);
    t.length = (uint32_t)(lx->cur - start);
    t.line   = line;
    t.column = col;
    t.lexeme = start;

    t.kind   = is_char ? TOK_CHAR_LITERAL : TOK_STRING_LITERAL;
    return t;
}

static Token lex_token_internal(Lexer *lx) {
    skip_whitespace_and_comments(lx);

    if (at_end(lx)) {
        Token t  = {0};
        t.kind   = TOK_EOF;
        t.offset = (uint32_t)(lx->src - lx->src);
        t.length = 0;
        t.line   = lx->line;
        t.column = lx->column;
        t.lexeme = lx->src;
        return t;
    }

    const char *start = lx->cur;
    uint32_t line     = lx->line;
    uint32_t col      = lx->column;
    char c            = advance_char(lx);

    if (isalpha((unsigned char)c) || c == '_') {
        return lex_identifier_or_keyword(lx, start, line, col);
    }

    if (isdigit((unsigned char)c)) {
        return lex_number(lx, start, line, col);
    }

    if (c == '\'' || c == '"') {
        return lex_string(lx, c, start, line, col);
    }

    TokenKind type = 0;
    int length     = 1;
    switch (c) {
    case '+':
        if (peek_char(lx) == '=') {
            advance_char(lx);
            type   = TOK_PLUS_EQ;
            length = 2;
        } else {
            type = TOK_PLUS;
        }
        break;
    case '-':
        if (peek_char(lx) == '>') {
            advance_char(lx);
            length = 2;
            type   = TOK_ARROW;
        }
        if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_MINUS_EQ;
        } else {
            type = TOK_MINUS;
        }
        break;
    case '*':
        if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_STAR_EQ;
        } else {
            type = TOK_STAR;
        }
        break;
    case '/':
        if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_SLASH_EQ;
        } else {
            type = TOK_SLASH;
        }
        break;
    case '%':
        if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_PERCENT_EQ;
        } else {
            type = TOK_PERCENT;
        }
        break;
    case '&':
        if (peek_char(lx) == '&') {
            advance_char(lx);
            length = 2;
            type   = TOK_AMP_AMP;
        } else if (peek_char(lx) == '=') {
            advance_char(lx);
            type = TOK_AMP_EQ;
        } else {
            type = TOK_AMP;
        }
        break;
    case '|':
        if (peek_char(lx) == '|') {
            advance_char(lx);
            length = 2;
            type   = TOK_PIPE_PIPE;
        } else if (peek_char(lx) == '=') {
            advance_char(lx);
            type = TOK_PIPE_EQ;
        } else {
            type = TOK_PIPE;
        }
        break;
    case '^':
        if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_CARET_EQ;
        } else {
            type = TOK_CARET;
        }
        break;
    case '~':
        type = TOK_TILDE;
        break;
    case '!':
        if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_BANG_EQ;
        } else {
            type = TOK_BANG;
        }
        break;
    case '=':
        if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_EQ_EQ;
        } else {
            type = TOK_EQ;
        }
        break;
    case '<':
        if (peek_char(lx) == '<') {
            advance_char(lx);
            length = 2;
            type   = TOK_SHL;
        } else if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_LT_EQ;
        } else {
            type = TOK_LT;
        }
        break;
    case '>':
        if (peek_char(lx) == '>') {
            advance_char(lx);
            length = 2;
            type   = TOK_SHR;
        } else if (peek_char(lx) == '=') {
            advance_char(lx);
            length = 2;
            type   = TOK_GT_EQ;
        } else {
            type = TOK_GT;
        }
        break;
    case '(':
        type = TOK_LPAREN;
        break;
    case ')':
        type = TOK_RPAREN;
        break;
    case '{':
        type = TOK_LBRACE;
        break;
    case '}':
        type = TOK_RBRACE;
        break;
    case '[':
        type = TOK_LBRACKET;
        break;
    case ']':
        type = TOK_RBRACKET;
        break;
    case ',':
        type = TOK_COMMA;
        break;
    case ';':
        type = TOK_SEMI;
        break;
    case ':':
        type = TOK_COLON;
        break;
    case '.':
        type = TOK_DOT;
        break;
    default:
        report(lx, "unexpected character");
        type = TOK_INVALID;
        break;
    }

    Token t;
    t.kind   = type;
    t.offset = (uint32_t)(start - lx->src);
    t.length = length;
    t.line   = line;
    t.column = col;
    t.lexeme = start;
    return t;
}

Token lexer_next(Lexer *lx) {
    if (lx->has_lookahead) {
        lx->has_lookahead = 0;
        return lx->lookahead;
    }
    return lex_token_internal(lx);
}

Token lexer_peek(Lexer *lx) {
    if (!lx->has_lookahead) {
        lx->lookahead     = lex_token_internal(lx);
        lx->has_lookahead = 1;
    }
    return lx->lookahead;
}

void lexer_consume(Lexer *lx) {
    if (lx->has_lookahead) {
        lx->has_lookahead = 0;
    } else {
        (void)lex_token_internal(lx);
    }
}

void lexer_dump(Lexer *lx) {
    for (;;) {
        Token t               = lexer_next(lx);

        const char *kind_name = token_kind_name(t.kind);

        printf("%4u:%-3u  %-16s  ", t.line, t.column, kind_name);

        // Print the lexeme safely using length.
        printf("'");
        fwrite(t.lexeme, 1, t.length, stdout);
        printf("'");

        // For integer literals, also show base/suffix to verify lexing logic.
        if (t.kind == TOK_INT_LITERAL) {
            printf("  [base=%u suffix=%s]", (unsigned)t.lit.int_literal.base,
                   int_suffix_name(t.lit.int_literal.suffix));
        }

        // For float literals, also show suffix to verify lexing logic.
        if (t.kind == TOK_FLOAT_LITERAL) {
            printf("  [suffix=%s]",
                   float_suffix_name(t.lit.float_literal.suffix));
        }

        printf("\n");

        if (t.kind == TOK_EOF) {
            break;
        }
    }
}
