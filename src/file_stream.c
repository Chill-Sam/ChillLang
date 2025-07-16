#pragma once

#include "int.c"

static int line = 1, column = 1;

/* syscall prototypes */
extern int open(const char *pathname, int flags, int mode);
extern ssize_t read(int fd, void *buf, size_t count);
extern ssize_t write(int fd, const void *buf, size_t count);
extern int close(int fd);

/* needed constants */
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define O_CREAT 0100  /* octal 0100 == decimal 64 */
#define O_TRUNC 01000 /* octal 01000 == decimal 512 */
#define STDOUT_FILENO 1
#define EOF (-1)

/* internal state */
static int in_fd = -1;
static int has_peeked = 0;
static unsigned char peek_buf;

/* initialize the lexer stream; returns 0 on success */
int init_stream(const char *filename) {
    in_fd = open(filename, O_RDONLY, 0);
    has_peeked = 0;
    return (in_fd < 0) ? -1 : 0;
}

/* close the stream */
void close_stream(void) {
    if (in_fd >= 0) {
        close(in_fd);
        in_fd = -1;
    }
}

/* peek the next byte (0–255) or EOF; does not advance */
int peek_char(void) {
    if (has_peeked) {
        return peek_buf;
    }
    if (in_fd < 0) {
        return EOF;
    }
    /* attempt to read one byte */
    if (read(in_fd, &peek_buf, 1) != 1) {
        return EOF;
    }
    has_peeked = 1;
    return peek_buf;
}

/* consume & return next byte (0–255) or EOF */
int next_char(void) {
    unsigned char c;
    if (has_peeked) {
        has_peeked = 0;
        c = peek_buf;
    } else {
        if (in_fd < 0 || read(in_fd, &c, 1) != 1) return EOF;
    }
    if (c == '\n') {
        line++;
        column = 1;
    } else {
        column++;
    }
    return c;
}
