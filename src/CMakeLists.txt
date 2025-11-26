set(SOURCES
    main.c
    lexer.c
    ast.c
    parser.c
    type.c
    symtab.c
    semantic.c
    ir.c
    ir_builder.c
    ir_phi.c
    lower_ir.c
    x64_codegen.c
)

add_executable(chc ${SOURCES})
