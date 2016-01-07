#define _GNU_SOURCE
#include <stdbool.h>
#include <assert.h>

#undef assert
#define assert(cond) if (!(cond)) { __asm__("int3"); __builtin_unreachable(); }

typedef __INT8_TYPE__  s8;
typedef __INT16_TYPE__ s16;
typedef __INT32_TYPE__ s32;
typedef __INT64_TYPE__ s64;

typedef __UINT8_TYPE__  u8;
typedef __UINT16_TYPE__ u16;
typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;

typedef double f64;

typedef _Bool b1;
typedef u32  b32;

typedef __SIZE_TYPE__  usize;
typedef s64 ssize;

static_assert(sizeof(usize) == sizeof(ssize), "Size types don't match");

static_assert(__SIZEOF_POINTER__ == 8, "Pointers aren't 64 bits");
static_assert(sizeof(f64) == 8, "f64 is the wrong size");

#define internal static
#define global_variable static

#define offsetof(type, member) __builtin_offsetof(type, member)

#define array_count(array) (sizeof(array) / sizeof((array)[0]))

#define HIGH_BIT ((u64)1 << 63)
