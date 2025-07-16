#pragma once

#include "string.c"

typedef unsigned long size_t;
typedef long ssize_t;

// 8-bit
typedef signed char int8_t;
typedef unsigned char uint8_t;

#define UINT8_MAX ((uint8_t)0xFFU)
#define INT8_MAX ((int8_t)0x7F)
#define INT8_MIN ((int8_t)(-INT8_MAX - 1))

// 16-bit
typedef signed short int16_t;
typedef unsigned short uint16_t;

#define UINT16_MAX ((uint16_t)0xFFFFU)
#define INT16_MAX ((int16_t)0x7FFF)
#define INT16_MIN ((int16_t)(-INT16_MAX - 1))

// 32-bit
typedef signed int int32_t;
typedef unsigned int uint32_t;

#define UINT32_MAX ((uint32_t)0xFFFFFFFFUL)
#define INT32_MAX ((int32_t)0x7FFFFFFF)
#define INT32_MIN ((int32_t)(-INT32_MAX - 1))

// 64-bit
typedef signed long long int64_t;
typedef unsigned long long uint64_t;

#define UINT64_MAX ((uint64_t)0xFFFFFFFFFFFFFFFFULL)
#define INT64_MAX ((int64_t)0x7FFFFFFFFFFFFFFFLL)
#define INT64_MIN ((int64_t)(-INT64_MAX - 1))

// Error return when parsing integers
enum IntParseErrorType {
    ERROR_INT_PARSE_OK,
    ERROR_INT_PARSE_SYNTAX,
    ERROR_INT_PARSE_OVERFLOW,
    ERROR_INT_PARSE_INPUT,
};

typedef struct {
    enum IntParseErrorType type;
    const char *msg;
} IntParseError;

// Parse param s with length len into a uint64_t and write to out_value
static IntParseError parse_unsigned(const char *s, int len,
                                    uint64_t *out_value) {
    if (!s || !out_value || len <= 0) {
        IntParseError err = {
            .type = ERROR_INT_PARSE_SYNTAX,
            .msg = "Syntax error: parse_unsigned() given invalid parameters\n"};
        return err;
    }

    uint64_t value = 0;
    for (int i = 0; i < len; i++) {
        char c = s[i];
        if (!is_digit(c)) {
            // non-digit in literal
            IntParseError err = {
                .type = ERROR_INT_PARSE_INPUT,
                .msg =
                    "Invalid input to parse_unsigned: non-digit in literal\n"};
            return err;
        }
        uint64_t digit = (uint64_t)(c - '0');
        // check: value*10 + digit <= UINT64_MAX
        if (value > (UINT64_MAX - digit) / 10ULL) {
            // overflow
            IntParseError err = {
                .type = ERROR_INT_PARSE_OVERFLOW,
                .msg =
                    "Integer overflow in parse_unsigned: max is UINT64_MAX\n"};
            return err;
        }
        value = value * 10ULL + digit;
    }

    *out_value = value;
    IntParseError err_ok = {.type = ERROR_INT_PARSE_OK, .msg = ""};
    return err_ok;
}

// Parse param s with length len into a int64_t and write to out_value
static IntParseError parse_signed(const char *s, int len, int64_t *out_value) {
    uint64_t uval;
    IntParseError res = parse_unsigned(s, len, &uval);
    if (res.type != ERROR_INT_PARSE_OK) {
        return res;
    }
    if (uval > (uint64_t)INT64_MAX) {
        // too large for signed 64-bit
        IntParseError err = {
            .type = ERROR_INT_PARSE_OVERFLOW,
            .msg = "Integer overflow in parse_signed: max in INT64_MAX\n"};
        return err;
    }

    *out_value = (int64_t)uval;
    IntParseError err_ok = {.type = ERROR_INT_PARSE_OK, .msg = ""};
    return err_ok;
}

static int in_range_signed(int64_t val, int64_t min, int64_t max) {
    return (val >= min && val <= max) ? 1 : 0;
}
static int in_range_unsigned(uint64_t val, uint64_t min, uint64_t max) {
    return (val >= min && val <= max) ? 1 : 0;
}
