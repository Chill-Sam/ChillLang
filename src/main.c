#include "AST/ast.c"
#include "AST/ast_log.c"
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
    ast_dump(program);
    semantic_check(program);
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
    // Emit our own _start:
    ir_safe_write("\n.section .text\n", 16);
    ir_safe_write(".global _start\n", 15);
    ir_safe_write("_start:\n", 8);

    // call main
    ir_safe_write("  call main\n", 12);

    // move main’s return (in RAX) → RDI
    ir_safe_write("  mov  %rax, %rdi\n", 18);

    // syscall number for exit is 60
    ir_safe_write("  mov  $60, %rax\n", 17);

    // do the syscall
    ir_safe_write("  syscall\n", 10);

    return 0;
}
