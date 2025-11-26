#include "ir.h"
#include "ir_phi.h"
#include "lexer.h"
#include "lower_ir.h"
#include "parser.h"
#include "semantic.h"
#include "x64_codegen.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PATH_MAX 64

static void usage(const char *prog) {
    fprintf(stderr, "Usage: %s <source.chl> -o <output>\n", prog);
    exit(1);
}

void cleanup(Lexer *lx, AstNode *root, IrModule *mod, char *src) {
    lexer_destroy(lx);
    free_translation_unit(root);
    free_ir_module(mod);
    free(src);
}

static void lexer_diag(void *user_data, uint32_t line, uint32_t column,
                       const char *msg) {
    const char *filename = (const char *)user_data;
    if (!filename) {
        filename = "<input>";
    }
    fprintf(stderr, "%s:%u:%u: lex error: %s\n", filename, line, column, msg);
}

static char *read_file(const char *path, size_t *out_size) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "error: failed to open input file '%s'\n", path);
        exit(1);
    }

    if (fseek(f, 0, SEEK_END) != 0) {
        fprintf(stderr, "error: fseek failed on '%s'\n", path);
        fclose(f);
        exit(1);
    }

    long sz = ftell(f);
    if (sz < 0) {
        fprintf(stderr, "error: ftell failed on '%s'\n", path);
        fclose(f);
        exit(1);
    }
    rewind(f);

    char *buf = malloc((size_t)sz + 1);
    if (!buf) {
        fprintf(stderr, "fatal: out of memory reading '%s'\n", path);
        fclose(f);
        exit(1);
    }

    size_t nread = fread(buf, 1, (size_t)sz, f);
    if (nread != (size_t)sz) {
        fprintf(stderr, "error: short read on '%s'\n", path);
        free(buf);
        fclose(f);
        exit(1);
    }

    buf[sz] = '\0';
    fclose(f);

    if (out_size) {
        *out_size = (size_t)sz;
    }
    return buf;
}

int main(int argc, char **argv) {
    // Argument parsing
    if (argc < 2) {
        usage(argv[0]);
    }

    const char *input_path  = NULL;
    const char *output_path = NULL;

    input_path              = argv[1];

    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0) {
            if (i + 1 >= argc) {
                usage(argv[0]);
            }
            output_path = argv[i + 1];
            i++;
        } else {
            usage(argv[0]);
        }
    }

    if (!output_path) {
        usage(argv[0]);
    }

    // Open input file

    size_t src_size = 0;
    char *src       = read_file(input_path, &src_size);

    // Lexer
    LexerConfig cfg = {
        .diag_fn   = lexer_diag,
        .diag_user = (void *)input_path,
    };

    Lexer *lx = lexer_create(src, src_size, cfg);
    if (!lx) {
        fprintf(stderr, "failed to create lexer\n");
        free(src);
        return 1;
    }

    // lexer_dump(lx);

    // Parser
    Parser p;
    parser_init(&p, lx);
    AstNode *root = parse_translation_unit(&p);

    fprintf(stdout, "Dumping AST: \n");
    ast_dump(root);
    fprintf(stdout, "-------------------------\n");

    // Semantic analysis
    sema_analyze(root);

    fprintf(stdout, "Semantic analysis finished\n");
    fprintf(stdout, "-------------------------\n");
    fprintf(stdout, "Dumping AST: \n");
    ast_dump(root);
    fprintf(stdout, "-------------------------\n");
    fprintf(stdout, "IR Lowering: \n");

    // IR lowering
    IrModule *mod = lower_to_ir(root);
    ir_dump_module(mod, stdout);

    fprintf(stdout, "-------------------------\n");
    fprintf(stdout, "Phi elimination pass\n");
    for (uint32_t i = 0; i < mod->funcs_count; i++) {
        IrFunc *fn = &mod->funcs[i];
        phi_elimination_pass(fn);
    }
    ir_dump_module(mod, stdout);
    fprintf(stdout, "-------------------------\n");

    // Code generation
    char asm_path[PATH_MAX];
    int n = snprintf(asm_path, sizeof(asm_path), "%s.s", output_path);
    if (n < 0 || (size_t)n >= sizeof(asm_path)) {
        fprintf(stderr, "error: output asm path too long\n");
        cleanup(lx, root, mod, src);
        exit(1);
    }

    FILE *asmf = fopen(asm_path, "w");
    if (!asmf) {
        fprintf(stderr, "error: failed to open output asm file '%s'\n",
                asm_path);
        cleanup(lx, root, mod, src);
        exit(1);
    }

    x64_emit_module(asmf, mod);
    fclose(asmf);

    char cmd[PATH_MAX * 2];
    n = snprintf(cmd, sizeof(cmd), "gcc -masm=intel -no-pie \"%s\" -o \"%s\"",
                 asm_path, output_path);
    if (n < 0 || (size_t)n >= sizeof(cmd)) {
        fprintf(stderr, "error: output command too long\n");
        cleanup(lx, root, mod, src);
        exit(1);
    }

    int rc = system(cmd);
    if (rc != 0) {
        fprintf(stderr, "error: failed to compile output file '%s'\n",
                output_path);
        cleanup(lx, root, mod, src);
        exit(1);
    }

    // Cleanup
    cleanup(lx, root, mod, src);
    return 0;
}
