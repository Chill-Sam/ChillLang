#include "AST/ast.c"
#include "AST/ast_log.c"
#include "as_ld.c"
#include "codegen.c"
#include "codegen.h"
#include "file_stream.c"
#include "ir.h"
#include "ir_print.c"
#include "irgen.c"
#include "lexer.c"
#include "parser.c"
#include "parser.h"
#include "semantic.c"

/* example usage: dump with peek info */
int main(int argc, char *argv[]) {
    if (argc < 2) {
        write(STDOUT_FILENO, "Usage: lexer_stream <file>\n", 27);
        return 1;
    }

    const char *filename = argv[1];
    lex_init(filename);

    // while (peek_token().type != TOKEN_EOF) {
    //     output_token(next_token());
    // }

    ASTNode *program = parse_program();

    if (lex_errors != 0 || parse_errors != 0) {
        return -1;
    }

    ast_dump(program);

    semantic_check(program);

    if (semantic_error_count != 0) {
        return -1;
    }

    ast_dump(program);

    IRProgram pr = {0};
    irgen_program(program, &pr);
    ir_print_program(&pr);

    out_fd = open("output.s", O_CREAT | O_TRUNC | O_WRONLY, 0644);
    for (size_t i = 0; i < pr.n_funcs; i++) {
        IRFunction *fn = pr.funcs[i];
        build_slots_for_function(fn);
        emit_prologue(fn);

        for (size_t i = 0; i < fn->n_code; i++) {
            lower_inst_to_asm(fn->code[i]);
        }

        ir_safe_write("  leave\n", 8);
        ir_safe_write("  ret\n", 6);
    }

    emit_start();
    output();
    return 0;
}
