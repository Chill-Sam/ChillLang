#include "symtab.h"

// ========== Type Table ==========

#define MAX_TYPES 64

static const char *type_names[MAX_TYPES];
static int num_types = 0;

int symtab_is_type(const char *name) {
    for (int i = 0; i < num_types; i++) {
        const char *t = type_names[i];
        int j         = 0;
        while (t[j] && name[j] && t[j] == name[j])
            j++;
        if (t[j] == '\0' && name[j] == '\0')
            return 1;
    }
    return 0;
}

void symtab_add_type(const char *name) {
    if (num_types < MAX_TYPES) {
        type_names[num_types++] = name;
    }
}

int symtab_same_type(const char *a, const char *b) {
    int i = 0;
    while (a[i] && b[i] && a[i] == b[i]) {
        i++;
    }
    return (a[i] == '\0' && b[i] == '\0');
}

// ========== Function Table ==========

#define MAX_FUNCS 256

typedef struct {
    const char *name;
    ASTNode *node; // AST_FUNCTION_DEF
} FuncEntry;

static FuncEntry functions[MAX_FUNCS];
static int num_functions = 0;

void symtab_add_function(ASTNode *fn_node) {
    const char *name = fn_node->first_child->next_sibling->data.text.name;

    // check for duplicate
    for (int i = 0; i < num_functions; i++) {
        if (functions[i].name && fn_node && name) {
            const char *a = functions[i].name;
            const char *b = name;
            int j         = 0;
            while (a[j] && b[j] && a[j] == b[j])
                j++;
            if (a[j] == '\0' && b[j] == '\0') {
                write(STDOUT_FILENO, "Error: duplicate function\n", 26);
                return;
            }
        }
    }

    if (num_functions < MAX_FUNCS) {
        functions[num_functions].name = name;
        functions[num_functions].node = fn_node;
        num_functions++;
    }
}

ASTNode *symtab_find_function(const char *name) {
    for (int i = 0; i < num_functions; i++) {
        const char *n = functions[i].name;
        int j         = 0;
        while (n[j] && name[j] && n[j] == name[j])
            j++;
        if (n[j] == '\0' && name[j] == '\0')
            return functions[i].node;
    }
    return 0;
}

// ========== Variable Table (Scoped) ==========

#define MAX_SCOPES         32
#define MAX_VARS_PER_SCOPE 64

typedef struct {
    const char *name;
    ASTNode *type_node;
    int is_mut;
} VarEntry;

typedef struct {
    VarEntry vars[MAX_VARS_PER_SCOPE];
    int count;
} Scope;

static Scope var_scopes[MAX_SCOPES];
static int current_scope = -1;

void symtab_push_scope(void) {
    if (current_scope + 1 >= MAX_SCOPES) {
        write(STDOUT_FILENO, "Error: too many nested scopes\n", 30);
        return;
    }
    current_scope++;
    var_scopes[current_scope].count = 0;
}

void symtab_pop_scope(void) {
    if (current_scope < 0) {
        write(STDOUT_FILENO, "Error: popping empty scope stack\n", 33);
        return;
    }
    current_scope--;
}

int symtab_add_var(const char *name, ASTNode *type_node, int is_mut) {
    // 1) Make sure we have an active, in‐bounds scope
    if (current_scope < 0) {
        write(STDOUT_FILENO, "Error: no active scope for variable\n", 36);
        return 0;
    }
    if (current_scope >= MAX_SCOPES) {
        write(STDOUT_FILENO, "Error: scope stack overflow\n", 28);
        return 0;
    }

    Scope *scope = &var_scopes[current_scope];

    // 2) Deduplicate in *this* scope
    for (int i = 0; i < scope->count; i++) {
        const char *n = scope->vars[i].name;
        int j         = 0;
        // simple strcmp
        while (n[j] && name[j] && n[j] == name[j])
            j++;
        if (n[j] == '\0' && name[j] == '\0') {
            // already declared here
            return 0;
        }
    }

    // 3) Check capacity
    if (scope->count >= MAX_VARS_PER_SCOPE) {
        write(STDOUT_FILENO, "Error: too many variables in one scope\n", 39);
        return 0;
    }

    // 4) Install the new entry
    VarEntry *v  = &scope->vars[scope->count++];
    v->name      = name;
    v->type_node = type_node;
    v->is_mut    = is_mut;
    return 1;
}

ASTNode *symtab_find_var(const char *name) {
    for (int s = current_scope; s >= 0; s--) {
        Scope *scope = &var_scopes[s];
        for (int i = 0; i < scope->count; i++) {
            const char *n = scope->vars[i].name;
            int j         = 0;
            while (n[j] && name[j] && n[j] == name[j])
                j++;
            if (n[j] == '\0' && name[j] == '\0') {
                return scope->vars[i].type_node;
            }
        }
    }
    return 0;
}

int symtab_is_mut(const char *name) {
    for (int s = current_scope; s >= 0; s--) {
        Scope *scope = &var_scopes[s];
        for (int i = 0; i < scope->count; i++) {
            const char *n = scope->vars[i].name;
            int j         = 0;
            while (n[j] && name[j] && n[j] == name[j])
                j++;
            if (n[j] == '\0' && name[j] == '\0') {
                return scope->vars[i].is_mut;
            }
        }
    }

    return 0;
}

// ========== Init & Reset ==========

void symtab_init(void) {
    num_types     = 0;
    num_functions = 0;
    current_scope = -1;

    symtab_add_type("u8");
    symtab_add_type("u16");
    symtab_add_type("u32");
    symtab_add_type("u64");
    symtab_add_type("i8");
    symtab_add_type("i16");
    symtab_add_type("i32");
    symtab_add_type("i64");
}

void symtab_reset(void) {
    symtab_init(); // just clear all state
}
