#pragma once

// Check if a character is alphabetic (A-Z or a-z)
int is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

// Check if a character is a digit (0-9)
int is_digit(char c) { return c >= '0' && c <= '9'; }

// Check if a character is alphanumeric (A-Z, a-z, or 0-9)
int is_alnum(char c) { return is_alpha(c) || is_digit(c); }

static int strlen(const char *s, int max) {
    int len = 0;
    while (len < max && s[len] != '\0') {
        len++;
    }
    return len;
}

// Returns 1 if str[0..len-1] exactly matches lit, and lit[len] == '\0'; 0
// otherwise.
static int str_eq_lit(const char *str, int len, const char *lit) {
    int i;
    for (i = 0; i < len; i++) {
        if (str[i] != lit[i]) return 0;
    }
    // ensure the literal has no extra characters
    return (lit[i] == '\0');
}

// --- tiny itoa: convert int → ASCII digits ---
// buf must be large enough for "-2147483648" + '\0', say 12 bytes.
static int itoa_int(int v, char *buf) {
    char *p = buf;

    int negative = 0;
    if (v < 0) {
        negative = 1;

        // careful: INT_MIN -> UB if you do -v, but for token types that's
        // unlikely
        v = -v;
    }
    // generate digits in reverse order
    do {
        int d = v % 10;
        *p++ = '0' + d;
        v /= 10;
    } while (v);
    if (negative) {
        *p++ = '-';
    }
    // reverse the buffer in-place
    int len = p - buf;
    for (int i = 0; i < len / 2; i++) {
        char tmp = buf[i];
        buf[i] = buf[len - 1 - i];
        buf[len - 1 - i] = tmp;
    }
    buf[len] = '\0';
    return len;
}

static int is_whitespace(char c) {
    switch (c) {
        case ' ':   // space
        case '\t':  // horizontal tab
        case '\n':  // newline (LF)
        case '\r':  // carriage return (CR)
        case '\v':  // vertical tab
        case '\f':  // form feed
            return 1;
        default:
            return 0;
    }
}
