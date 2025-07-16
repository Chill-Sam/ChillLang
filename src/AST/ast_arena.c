#pragma once

#include "../int.c"

#define NULL ((void *)0)
#define AST_ARENA_SIZE (1 << 20)  // 1 MiB arena

// The raw buffer and current offset
static unsigned char _ast_arena[AST_ARENA_SIZE];
static size_t _ast_off = 0;

// Helper to round `n` up to the next multiple of `align`.
// `align` must be a power of two.
static size_t align_up(size_t n, size_t align) {
    return (n + align - 1) & ~(align - 1);
}

void ast_arena_reset(void) { _ast_off = 0; }

void *ast_arena_alloc(size_t bytes) {
    // always align to pointer size for safe storage of ASTNode, strings, etc.
    const size_t align = sizeof(void *);
    size_t offset = align_up(_ast_off, align);

    // out-of-memory check
    if (offset + bytes > AST_ARENA_SIZE) {
        // OOM: you can handle this more gracefully if desired
        return NULL;
    }

    void *ptr = (void *)(_ast_arena + offset);
    _ast_off = offset + bytes;
    return ptr;
}

// Copy the first `len` bytes of `s` into the arena, NUL-terminate, and return
// the new pointer.
static char *ast_strdup(const char *s, int len) {
    // allocate len+1 bytes from the arena
    char *buf = ast_arena_alloc(len + 1);
    // copy the characters
    for (int i = 0; i < len; i++) {
        buf[i] = s[i];
    }
    // NUL-terminate
    buf[len] = '\0';
    return buf;
}
