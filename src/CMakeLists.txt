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
    lower_ir.c
)

add_executable(chc ${SOURCES})
