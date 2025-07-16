#pragma once

#include "file_stream.c"
#include "int.c"
#include "string.c"
#include "tokens.c"

#define NUM_SIZE 128
#define NUM_SUFFIX_SIZE 8
#define WORD_SIZE 128

#define MAX_LOOKAHEAD 4  // how many tokens you ever want to peek
static Token lookahead_buf[MAX_LOOKAHEAD];
static int lookahead_count = 0;

static Token lex_one_token(void);  // core lexing logic

int parse_errors = 0;

// Will lex a number starting at current location in input stream
// Returns a Token based on number
// Assumes currently starting on a number literal
Token lex_number_literal(void) {
    char number[NUM_SIZE + 1];
    size_t n_number = 0;
    while (n_number < NUM_SIZE && is_digit(peek_char())) {
        number[n_number] = (unsigned char)next_char();
        n_number++;
    }
    number[n_number] = '\0';
    // write(STDOUT_FILENO, number, n_number);
    // write(STDOUT_FILENO, ": number\n", 9);

    char suffix[NUM_SUFFIX_SIZE + 1];
    size_t n_suffix = 0;
    while (n_suffix < NUM_SUFFIX_SIZE && is_alnum(peek_char())) {
        suffix[n_suffix] = (unsigned char)next_char();
        n_suffix++;
    }
    suffix[n_suffix] = '\0';
    // write(STDOUT_FILENO, suffix, n_suffix);
    // write(STDOUT_FILENO, ": suffix\n", 9);

    // Generate token
    Token tok;
    tok.line = line;
    tok.column = column;

    uint64_t val = 0;
    IntParseError err = parse_unsigned(number, n_number, &val);
    if (err.type != ERROR_INT_PARSE_OK) {
        size_t len = strlen(err.msg, 64);
        write(STDOUT_FILENO, err.msg, len);
        return make_error_token(line, column, number, n_number, err.msg);
    }

    // Handle no suffix
    if (suffix[0] == '\0') {
        if (val <= (uint64_t)INT32_MAX + 1) {
            tok.type = TOKEN_INT_LITERAL;
            tok.data.literal.i = (int32_t)val;
        } else if (val <= (uint64_t)INT64_MAX + 1) {
            tok.type = TOKEN_INT_LITERAL;
            tok.data.literal.i = (int64_t)val;
        } else {
            // Should never happen due to previous overflow check
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than INT64_MAX\n");
        }
    } else if (str_eq_lit(suffix, 2, "u8")) {
        if (val > UINT8_MAX) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than UINT8_MAX\n");
        }
        tok.type = TOKEN_U8_LITERAL;
        tok.data.literal.u = (uint64_t)val;
    } else if (str_eq_lit(suffix, 3, "u16")) {
        if (val > UINT16_MAX) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than UINT16_MAX\n");
        }
        tok.type = TOKEN_U16_LITERAL;
        tok.data.literal.u = (uint64_t)val;
    } else if (str_eq_lit(suffix, 3, "u32")) {
        if (val > UINT32_MAX) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than UINT32_MAX\n");
        }
        tok.type = TOKEN_U32_LITERAL;
        tok.data.literal.u = (uint64_t)val;
    } else if (str_eq_lit(suffix, 3, "u64")) {
        if (val > UINT64_MAX) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than UINT64_MAX\n");
        }
        tok.type = TOKEN_U64_LITERAL;
        tok.data.literal.u = (uint64_t)val;
    } else if (str_eq_lit(suffix, 2, "i8")) {
        if (val > (uint64_t)(INT8_MAX) + 1) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than INT8_MAX\n");
        }
        tok.type = TOKEN_I8_LITERAL;
        tok.data.literal.i = (uint64_t)val;
    } else if (str_eq_lit(suffix, 3, "i16")) {
        if (val > (uint64_t)(INT16_MAX) + 1) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than INT16_MAX\n");
        }
        tok.type = TOKEN_I16_LITERAL;
        tok.data.literal.i = (uint64_t)val;
    } else if (str_eq_lit(suffix, 3, "i32")) {
        if (val > (uint64_t)(INT32_MAX) + 1) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than INT32_MAX\n");
        }
        tok.type = TOKEN_I32_LITERAL;
        tok.data.literal.i = (uint64_t)val;
    } else if (str_eq_lit(suffix, 3, "i64")) {
        if (val > (uint64_t)(INT64_MAX) + 1) {
            return make_error_token(line, column, number, n_number,
                                    "Error: Number larger than INT64_MAX\n");
        }
        tok.type = TOKEN_I64_LITERAL;
        tok.data.literal.i = (uint64_t)val;
    } else {
        return make_error_token(line, column, suffix, n_suffix,
                                "Error: Invalid suffix\n");
    }

    tok.length = n_number + n_suffix;
    // Copy number + suffix to lexeme
    int pos = 0;
    while (pos < n_number && pos < MAX_LEXEME) {
        tok.lexeme[pos] = number[pos];
        pos++;
    }
    while (pos - n_number < n_suffix && pos < MAX_LEXEME) {
        tok.lexeme[pos] = suffix[pos - n_number];
        pos++;
    }
    tok.lexeme[pos] = '\0';

    return tok;
}

Token lex_char(void) {
    unsigned char c = (unsigned char)next_char();

    Token tok;
    tok.line = line;
    tok.column = column;
    tok.length = 1;
    tok.lexeme[0] = c;
    tok.lexeme[1] = '\0';

    // Parse singular chars
    switch (c) {
        case '(':
            tok.type = TOKEN_LPAREN;
            break;
        case ')':
            tok.type = TOKEN_RPAREN;
            break;
        case '{':
            tok.type = TOKEN_LBRACE;
            break;
        case '}':
            tok.type = TOKEN_RBRACE;
            break;
        case ';':
            tok.type = TOKEN_SEMI;
            break;
        case '+':
            tok.type = TOKEN_PLUS;
            break;
        case '-':
            tok.type = TOKEN_MINUS;
            break;
        case '*':
            tok.type = TOKEN_STAR;
            break;
        case '/':
            tok.type = TOKEN_SLASH;
            break;
        case '%':
            tok.type = TOKEN_PERCENT;
            break;
        case '=':
            tok.type = TOKEN_EQUALS;
            break;
        case ',':
            tok.type = TOKEN_COMMA;
            break;
        default:
            tok = make_error_token(line, column, &c, 1,
                                   "Error: Unknown character while lexing\n");
    }

    return tok;
}

Token lex_word(void) {
    char word[WORD_SIZE + 1];
    size_t n = 0;
    while (n < WORD_SIZE && is_alnum(peek_char())) {
        word[n] = (unsigned char)next_char();
        n++;
    }
    word[n] = '\0';

    Token tok;
    tok.line = line;
    tok.column = column;
    tok.length = n;

    int pos = 0;
    while (pos < n && pos < MAX_LEXEME) {
        tok.lexeme[pos] = word[pos];
        pos++;
    }
    tok.lexeme[pos] = '\0';

    // if (str_eq_lit(word, 2, "u8")) {
    //     tok.type = TOKEN_U8;
    // } else if (str_eq_lit(word, 3, "u16")) {
    //     tok.type = TOKEN_U16;
    // } else if (str_eq_lit(word, 3, "u32")) {
    //     tok.type = TOKEN_U32;
    // } else if (str_eq_lit(word, 3, "u64")) {
    //     tok.type = TOKEN_U64;
    // } else if (str_eq_lit(word, 2, "i8")) {
    //     tok.type = TOKEN_I8;
    // } else if (str_eq_lit(word, 3, "i16")) {
    //     tok.type = TOKEN_I16;
    // } else if (str_eq_lit(word, 3, "i32")) {
    //     tok.type = TOKEN_I32;
    // } else if (str_eq_lit(word, 3, "i64")) {
    //     tok.type = TOKEN_I64;
    // }
    if (str_eq_lit(word, 6, "return")) {
        tok.type = TOKEN_RETURN;
    } else if (str_eq_lit(word, 3, "mut")) {
        tok.type = TOKEN_MUT;
    } else {
        tok.type = TOKEN_IDENTIFIER;
    }

    return tok;
}

int lex_init(const char *filename) {
    parse_errors = 0;
    lookahead_count = 0;
    return init_stream(filename);  // your existing init_stream()
}
void lex_close(void) { close_stream(); }

Token next_token(void) {
    if (lookahead_count > 0) {
        Token t = lookahead_buf[0];

        // Shift buf down
        for (int i = 1; i < lookahead_count; i++) {
            lookahead_buf[i - 1] = lookahead_buf[i];
        }
        lookahead_count--;
        return t;
    }

    return lex_one_token();
}

Token peek_nth(int n) {
    if (n < 1 || n > MAX_LOOKAHEAD) {
        write(STDOUT_FILENO, "Error, invalid peek\n", 20);
        Token eof = {.type = TOKEN_EOF};
        return eof;
    }

    while (lookahead_count < n) {
        if (lookahead_count >= MAX_LOOKAHEAD) {
            write(STDOUT_FILENO, "Error: lookahead overflow\n", 26);
            break;
        }

        lookahead_buf[lookahead_count++] = lex_one_token();
    }

    return lookahead_buf[n - 1];
}

Token peek_token(void) { return peek_nth(1); }

static Token lex_one_token(void) {
    int c = peek_char();
    if (c == EOF) {
        Token eof = {.type = TOKEN_EOF};
        return eof;
    } else if (is_digit(c)) {
        return lex_number_literal();
    } else if (is_alnum(c)) {
        return lex_word();
    } else if (!is_whitespace(c)) {
        return lex_char();
    } else {
        next_char();  // Consume whitespace
        return lex_one_token();
    }
}

// “accept”—if lookahead is t, consume & return true
static int accept(enum TokenType t) {
    if (peek_token().type == t) {
        next_token();
        return 1;
    }
    return 0;
}

static Token expect(enum TokenType t) {
    Token tk = next_token();
    if (tk.type != t) {
        parse_errors++;
        write(STDOUT_FILENO, "Unexpected token\n", 17);
        output_token(tk);
    }
    return tk;
}
