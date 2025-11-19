#include "ir.h"
#include "lexer.h"
#include "lower_ir.h"
#include "parser.h"
#include "semantic.h"
#include <stdio.h>
#include <stdlib.h>

static void lexer_diag(void *user_data, uint32_t line, uint32_t column,
                       const char *msg) {
    const char *filename = (const char *)user_data;
    if (!filename) {
        filename = "<input>";
    }
    fprintf(stderr, "%s:%u:%u: lex error: %s\n", filename, line, column, msg);
}

static char *read_file(const char *path, size_t *out_len) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        perror(path);
        return NULL;
    }

    if (fseek(f, 0, SEEK_END) != 0) {
        perror("fseek");
        fclose(f);
        return NULL;
    }

    long sz = ftell(f);
    if (sz < 0) {
        perror("ftell");
        fclose(f);
        return NULL;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        perror("fseek");
        fclose(f);
        return NULL;
    }

    char *buf = malloc((size_t)sz);
    if (!buf) {
        fprintf(stderr, "out of memory\n");
        fclose(f);
        return NULL;
    }

    size_t nread = fread(buf, 1, (size_t)sz, f);
    if (nread != (size_t)sz) {
        fprintf(stderr, "short read\n");
        free(buf);
        fclose(f);
        return NULL;
    }

    fclose(f);
    *out_len = (size_t)sz;
    return buf;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <source-file>\n", argv[0]);
        return 1;
    }

    const char *filename = argv[1];

    size_t len           = 0;
    char *src            = read_file(filename, &len);
    if (!src) {
        return 1;
    }

    LexerConfig cfg = {
        .diag_fn   = lexer_diag,
        .diag_user = (void *)filename,
    };

    Lexer *lx = lexer_create(src, len, cfg);
    if (!lx) {
        fprintf(stderr, "failed to create lexer\n");
        free(src);
        return 1;
    }

    // lexer_dump(lx);

    Parser p;
    parser_init(&p, lx);
    AstNode *root = parse_translation_unit(&p);

    fprintf(stderr, "Dumping AST: \n");

    ast_dump(root);

    fprintf(stderr, "\n-------------------------\n");

    sema_analyze(root);

    fprintf(stderr, "Semantic analysis finished\n");
    fprintf(stderr, "-------------------------\n");

    IrModule *mod = lower_to_ir(root);
    ir_dump_module(mod, stdout);

    lexer_destroy(lx);
    free_translation_unit(root);
    free_ir_module(mod);
    free(src);
    return 0;
}
