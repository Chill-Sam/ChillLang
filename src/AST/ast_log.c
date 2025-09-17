#pragma once

#include "../file_stream.c"
#include "../int.c"
#include "ast.c"

// -----------------------------------------------------------------------------
// AST Pretty-Printer / Dumper (no external libraries)
// -----------------------------------------------------------------------------

// If your AST_KIND_COUNT isn’t 15, adjust this array
static const char *ast_kind_names[] = {
    "PROGRAM",     "FUNCTION_DEF", "PARAM_LIST",  "PARAM",       "BLOCK",
    "RETURN_STMT", "CONST_DECL",   "MUT_DECL",    "EXPR_STMT",   "ASSIGN_STMT",
    "EXPRESSION",  "ADD_EXPR",     "SUB_EXPR",    "MUL_EXPR",    "DIV_EXPR",
    "MOD_EXPR",    "NEG_EXPR",     "BW_NOT_EXPR", "SHIFT_RIGHT", "SHIFT_LEFT",
    "BW_AND_EXPR", "BW_XOR_EXPR",  "BW_OR_EXPR",  "CALL_EXPR",   "ARG_LIST",
    "INT_LITERAL", "IDENTIFIER",   "TYPE_NAME"};

// compute length of a NUL-terminated string
inline static int str_len(const char *s) {
    int i = 0;
    while (s[i] != '\0') i++;
    return i;
}

// reverse buf[0..len-1]
static void reverse_buf(char *buf, int len) {
    for (int i = 0; i < len / 2; i++) {
        char tmp = buf[i];
        buf[i] = buf[len - 1 - i];
        buf[len - 1 - i] = tmp;
    }
}

// convert unsigned 64→decimal ASCII in buf; returns number of chars (no NUL)
static int utoa64(uint64_t v, char *buf) {
    int pos = 0;
    if (v == 0) {
        buf[pos++] = '0';
    } else {
        while (v > 0) {
            buf[pos++] = '0' + (v % 10);
            v /= 10;
        }
    }
    reverse_buf(buf, pos);
    return pos;
}

// convert signed 64→decimal ASCII in buf; returns number of chars (no NUL)
static int itoa64(int64_t v, char *buf) {
    int pos = 0;
    uint64_t u;
    if (v < 0) {
        buf[pos++] = '-';
        // avoid overflow on INT64_MIN
        u = (uint64_t)(-(v + 1)) + 1;
    } else {
        u = (uint64_t)v;
    }
    int start = pos;
    if (u == 0) {
        buf[pos++] = '0';
    } else {
        while (u > 0) {
            buf[pos++] = '0' + (u % 10);
            u /= 10;
        }
    }
    // reverse only the digits (not any leading '-')
    reverse_buf(buf + start, pos - start);
    return pos;
}

// write exactly n bytes, retrying if needed
static void safe_write(const char *buf, int n) {
    int written = 0;
    while (written < n) {
        int w = write(STDOUT_FILENO, buf + written, n - written);
        if (w <= 0) break;
        written += w;
    }
}

// print '  ' repeated depth times
static void print_indent(int depth) {
    for (int i = 0; i < depth; i++) {
        safe_write("  ", 2);
    }
}

// forward
typedef struct ASTNode ASTNode;

// recursive dump
static void dump_rec(ASTNode *node, int depth) {
    if (!node) return;

    // 1) indent + kind
    print_indent(depth);
    const char *kname = ast_kind_names[node->kind];
    safe_write(kname, str_len(kname));

    // 2) location
    safe_write(" (", 2);
    char tmp[32];
    int n = itoa64(node->line, tmp);
    safe_write(tmp, n);
    safe_write(":", 1);
    n = itoa64(node->column, tmp);
    safe_write(tmp, n);
    safe_write(")", 1);

    // 3) extra payload
    switch (node->kind) {
        case AST_IDENTIFIER:
        case AST_TYPE_NAME: {
            safe_write(" name=", 6);
            safe_write(node->data.text.name, str_len(node->data.text.name));
            break;
        }
        case AST_INT_LITERAL: {
            safe_write(" value=", 7);
            // choose signed or unsigned
            if (node->data.int_lit.u_val == 0 && node->data.int_lit.i_val < 0) {
                // negative
                n = itoa64(node->data.int_lit.i_val, tmp);
            } else if (node->data.int_lit.u_val == 0 &&
                       node->data.int_lit.i_val >= 0) {
                // non-negative signed
                n = itoa64(node->data.int_lit.i_val, tmp);
            } else {
                // unsigned
                n = utoa64(node->data.int_lit.u_val, tmp);
            }
            safe_write(tmp, n);

            if (!node->data.int_lit.type) {
                break;
            }
            safe_write(" type=", 6);
            safe_write(node->data.int_lit.type->data.text.name,
                       str_len(node->data.int_lit.type->data.text.name));
            break;
        }
        default:
            break;
    }

    // newline
    safe_write("\n", 1);

    // 4) recurse children
    ASTNode *c = node->first_child;
    while (c) {
        dump_rec(c, depth + 1);
        c = c->next_sibling;
    }
}

// public entrypoint
void ast_dump(ASTNode *root) { dump_rec(root, 0); }
