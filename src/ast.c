#include "ast.h"
#include <stdio.h>
#include <stdlib.h>

void ast_list_init(AstNodeList *list) {
    list->items    = NULL;
    list->count    = 0;
    list->capacity = 0;
}

// Internal growth helper
static void ast_list_reserve(AstNodeList *list, uint32_t new_cap) {
    AstNode **new_items = realloc(list->items, new_cap * sizeof(AstNode *));
    if (!new_items) {
        fprintf(stderr, "fatal: out of memory in ast_list_reserve\n");
        abort();
    }
    list->items    = new_items;
    list->capacity = new_cap;
}

void ast_list_push(AstNodeList *list, AstNode *node) {
    if (list->count == list->capacity) {
        uint32_t new_cap = list->capacity ? list->capacity * 2 : 4;
        ast_list_reserve(list, new_cap);
    }
    list->items[list->count++] = node;
}

void ast_list_free(AstNodeList *list) {
    free(list->items);
    list->items    = NULL;
    list->count    = 0;
    list->capacity = 0;
}

static void print_indent(int indent) {
    for (int i = 0; i < indent; i++) {
        fputs("  ", stdout);
    }
}

static void print_token_lexeme(const Token *tok) {
    fwrite(tok->lexeme, 1, tok->length, stdout);
}

static const char *binop_name(AstBinOp op) {
    switch (op) {
    case BIN_ADD:
        return "+";
    case BIN_SUB:
        return "-";
    case BIN_MUL:
        return "*";
    case BIN_DIV:
        return "/";
    case BIN_MOD:
        return "%";

    case BIN_LT:
        return "<";
    case BIN_GT:
        return ">";
    case BIN_LE:
        return "<=";
    case BIN_GE:
        return ">=";
    case BIN_EQ:
        return "==";
    case BIN_NE:
        return "!=";

    case BIN_BIT_AND:
        return "&";
    case BIN_BIT_XOR:
        return "^";
    case BIN_BIT_OR:
        return "|";

    case BIN_SHL:
        return "<<";
    case BIN_SHR:
        return ">>";

    case BIN_AND:
        return "&&";
    case BIN_OR:
        return "||";
    }
    return "?";
}

static const char *unop_name(AstUnaryOp op) {
    switch (op) {
    case UN_NEG:
        return "-";
    case UN_NOT:
        return "!";
    case UN_BITNOT:
        return "~";
    }
    return "?";
}

static void ast_dump_node(AstNode *node, int indent);

static void dump_node_list(const char *label, AstNodeList *list, int indent) {
    if (list->count == 0)
        return;

    print_indent(indent);
    printf("%s:\n", label);

    for (uint32_t i = 0; i < list->count; i++) {
        ast_dump_node(list->items[i], indent + 1);
    }
}

static void ast_dump_translation_unit(AstNode *node, int indent) {
    print_indent(indent);
    puts("TranslationUnit");
    dump_node_list("items", &node->as.translation_unit.items, indent + 1);
}

static void ast_dump_func_decl(AstNode *node, int indent) {
    print_indent(indent);
    fputs("FuncDecl name = ", stdout);
    print_token_lexeme(&node->as.func_decl.name);
    fputc('\n', stdout);

    if (node->as.func_decl.return_type) {
        print_indent(indent + 1);
        fputs("ReturnType:\n", stdout);
        ast_dump_node(node->as.func_decl.return_type, indent + 2);
    }

    dump_node_list("params", &node->as.func_decl.params, indent + 1);

    if (node->as.func_decl.body) {
        print_indent(indent + 1);
        fputs("Body:\n", stdout);
        ast_dump_node(node->as.func_decl.body, indent + 2);
    }
}

static void ast_dump_param(AstNode *node, int indent) {
    print_indent(indent);
    fputs("Param ", stdout);
    if (node->as.param.is_mut)
        fputs("[mut] ", stdout);
    fputs("name = ", stdout);
    print_token_lexeme(&node->as.param.name);
    fputc('\n', stdout);

    print_indent(indent + 1);
    fputs("Type:\n", stdout);
    ast_dump_node(node->as.param.type, indent + 2);
}

static void ast_dump_struct_decl(AstNode *node, int indent) {
    print_indent(indent);
    fputs("StructDecl name = ", stdout);
    print_token_lexeme(&node->as.struct_decl.name);
    fputc('\n', stdout);

    dump_node_list("fields", &node->as.struct_decl.fields, indent + 1);
}

static void ast_dump_field(AstNode *node, int indent) {
    print_indent(indent);
    fputs("Field name = ", stdout);
    print_token_lexeme(&node->as.field.name);
    fputc('\n', stdout);

    print_indent(indent + 1);
    fputs("Type:\n", stdout);
    ast_dump_node(node->as.field.type, indent + 2);
}

static void ast_dump_block_stmt(AstNode *node, int indent) {
    print_indent(indent);
    puts("BlockStmt");
    dump_node_list("stmts", &node->as.block_stmt.stmts, indent + 1);
}

static void ast_dump_var_decl(AstNode *node, int indent) {
    print_indent(indent);
    fputs("VarDecl ", stdout);
    if (node->as.var_decl.is_mut)
        fputs("[mut] ", stdout);
    fputs("name = ", stdout);
    print_token_lexeme(&node->as.var_decl.name);
    fputc('\n', stdout);

    print_indent(indent + 1);
    fputs("Type:\n", stdout);
    ast_dump_node(node->as.var_decl.type, indent + 2);

    if (node->as.var_decl.init) {
        print_indent(indent + 1);
        fputs("Init:\n", stdout);
        ast_dump_node(node->as.var_decl.init, indent + 2);
    }
}

static void ast_dump_assign_expr(AstNode *node, int indent) {
    print_indent(indent);
    fputs("AssignExpr op = ", stdout);
    switch (node->as.assign_expr.op) {
    case ASSIGN_EQ:
        fputs("=", stdout);
        break;
    default:
        fputs("<?>", stdout);
        break;
    }
    fputc('\n', stdout);

    print_indent(indent + 1);
    fputs("LHS:\n", stdout);
    ast_dump_node(node->as.assign_expr.lhs, indent + 2);

    print_indent(indent + 1);
    fputs("RHS:\n", stdout);
    ast_dump_node(node->as.assign_expr.rhs, indent + 2);
}

static void ast_dump_return_stmt(AstNode *node, int indent) {
    print_indent(indent);
    puts("ReturnStmt");
    if (node->as.return_stmt.expr) {
        print_indent(indent + 1);
        fputs("Expr:\n", stdout);
        ast_dump_node(node->as.return_stmt.expr, indent + 2);
    }
}

static void ast_dump_if_stmt(AstNode *node, int indent) {
    print_indent(indent);
    puts("IfStmt");
    if (node->as.if_stmt.cond) {
        print_indent(indent + 1);
        fputs("Cond:\n", stdout);
        ast_dump_node(node->as.if_stmt.cond, indent + 2);
    }
    if (node->as.if_stmt.then_block) {
        print_indent(indent + 1);
        fputs("Then:\n", stdout);
        ast_dump_node(node->as.if_stmt.then_block, indent + 2);
    }
    if (node->as.if_stmt.else_block) {
        print_indent(indent + 1);
        fputs("Else:\n", stdout);
        ast_dump_node(node->as.if_stmt.else_block, indent + 2);
    }
}

static void ast_dump_while_stmt(AstNode *node, int indent) {
    print_indent(indent);
    puts("WhileStmt");
    if (node->as.while_stmt.cond) {
        print_indent(indent + 1);
        fputs("Cond:\n", stdout);
        ast_dump_node(node->as.while_stmt.cond, indent + 2);
    }
    if (node->as.while_stmt.body) {
        print_indent(indent + 1);
        fputs("Body:\n", stdout);
        ast_dump_node(node->as.while_stmt.body, indent + 2);
    }
}

static void ast_dump_expr_stmt(AstNode *node, int indent) {
    print_indent(indent);
    puts("ExprStmt");
    if (node->as.expr_stmt.expr) {
        print_indent(indent + 1);
        fputs("Expr:\n", stdout);
        ast_dump_node(node->as.expr_stmt.expr, indent + 2);
    }
}

static void ast_dump_bin_expr(AstNode *node, int indent) {
    print_indent(indent);
    fputs("BinExpr op = ", stdout);
    fputs(binop_name(node->as.bin_expr.op), stdout);
    fputc('\n', stdout);

    print_indent(indent + 1);
    fputs("LHS:\n", stdout);
    ast_dump_node(node->as.bin_expr.lhs, indent + 2);

    print_indent(indent + 1);
    fputs("RHS:\n", stdout);
    ast_dump_node(node->as.bin_expr.rhs, indent + 2);
}

static void ast_dump_unary_expr(AstNode *node, int indent) {
    print_indent(indent);
    fputs("UnaryExpr op = ", stdout);
    fputs(unop_name(node->as.unary_expr.op), stdout);
    fputc('\n', stdout);

    print_indent(indent + 1);
    fputs("Expr:\n", stdout);
    ast_dump_node(node->as.unary_expr.expr, indent + 2);
}

static void ast_dump_call_expr(AstNode *node, int indent) {
    print_indent(indent);
    puts("CallExpr");

    print_indent(indent + 1);
    fputs("Callee:\n", stdout);
    ast_dump_node(node->as.call_expr.callee, indent + 2);

    dump_node_list("args", &node->as.call_expr.args, indent + 1);
}

static void ast_dump_cast_expr(AstNode *node, int indent) {
    print_indent(indent);
    fprintf(stdout, "CastExpr target = %d\n", node->as.cast_expr.target_type);

    print_indent(indent + 1);
    fputs("Expr:\n", stdout);
    ast_dump_node(node->as.cast_expr.expr, indent + 2);
}

static void ast_dump_member_expr(AstNode *node, int indent) {
    print_indent(indent);
    fputs("MemberExpr field = ", stdout);
    print_token_lexeme(&node->as.member_expr.field);
    fputc('\n', stdout);

    print_indent(indent + 1);
    fputs("Base:\n", stdout);
    ast_dump_node(node->as.member_expr.base, indent + 2);
}

static void ast_dump_ident_expr(AstNode *node, int indent) {
    print_indent(indent);
    fputs("IdentExpr name = ", stdout);
    print_token_lexeme(&node->as.ident_expr.name);
    fputc('\n', stdout);
}

static void ast_dump_literal_expr(AstNode *node, int indent) {
    print_indent(indent);
    fputs("LiteralExpr tok = '", stdout);
    print_token_lexeme(&node->as.literal_expr.tok);
    fputs("'\n", stdout);
}

static void ast_dump_node(AstNode *node, int indent) {
    if (!node) {
        print_indent(indent);
        puts("<null>");
        return;
    }

    switch (node->kind) {
    case AST_TRANSLATION_UNIT:
        ast_dump_translation_unit(node, indent);
        break;

    case AST_FUNC_DECL:
        ast_dump_func_decl(node, indent);
        break;

    case AST_PARAM:
        ast_dump_param(node, indent);
        break;

    case AST_STRUCT_DECL:
        ast_dump_struct_decl(node, indent);
        break;

    case AST_FIELD:
        ast_dump_field(node, indent);
        break;

    case AST_BLOCK_STMT:
        ast_dump_block_stmt(node, indent);
        break;

    case AST_VAR_DECL:
        ast_dump_var_decl(node, indent);
        break;

    case AST_RETURN_STMT:
        ast_dump_return_stmt(node, indent);
        break;

    case AST_EXPR_STMT:
        ast_dump_expr_stmt(node, indent);
        break;

    case AST_ASSIGN_EXPR:
        ast_dump_assign_expr(node, indent);
        break;

    case AST_IF_STMT:
        ast_dump_if_stmt(node, indent);
        break;

    case AST_WHILE_STMT:
        ast_dump_while_stmt(node, indent);
        break;

    case AST_BIN_EXPR:
        ast_dump_bin_expr(node, indent);
        break;

    case AST_UNARY_EXPR:
        ast_dump_unary_expr(node, indent);
        break;

    case AST_CALL_EXPR:
        ast_dump_call_expr(node, indent);
        break;

    case AST_CAST_EXPR:
        ast_dump_cast_expr(node, indent);
        break;

    case AST_MEMBER_EXPR:
        ast_dump_member_expr(node, indent);
        break;

    case AST_IDENT_EXPR:
        ast_dump_ident_expr(node, indent);
        break;

    case AST_LITERAL_EXPR:
        ast_dump_literal_expr(node, indent);
        break;

    default:
        print_indent(indent);
        puts("<unknown AstKind>");
        break;
    }
}

void ast_dump(AstNode *node) { ast_dump_node(node, 0); }
