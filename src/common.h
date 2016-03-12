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
typedef u16   b16;
typedef u32   b32;

typedef __SIZE_TYPE__  usize;
typedef s64 ssize;

static_assert(sizeof(usize) == sizeof(ssize), "Size types don't match");

static_assert(__SIZEOF_POINTER__ == 8, "Pointers aren't 64 bits");
static_assert(sizeof(f64) == 8, "f64 is the wrong size");

#define internal static
#define global_variable static

#define offsetof(type, member) __builtin_offsetof(type, member)

#define array_count(array) (sizeof(array) / sizeof((array)[0]))

#define CAT_(x, y) x##y
#define CAT(x, y) CAT_(x, y)

#define VA_NARG(...) \
         VA_NARG_(__VA_ARGS__,                   \
                  63,62,61,60,                   \
                  59,58,57,56,55,54,53,52,51,50, \
                  49,48,47,46,45,44,43,42,41,40, \
                  39,38,37,36,35,34,33,32,31,30, \
                  29,28,27,26,25,24,23,22,21,20, \
                  19,18,17,16,15,14,13,12,11,10, \
                  9,8,7,6,5,4,3,2,1,0)
#define VA_NARG_(...) \
         VA_ARG_N(__VA_ARGS__)
#define VA_ARG_N( \
          _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
         _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
         _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
         _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
         _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
         _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
         _61,_62,_63,N,...) N

#define HIGH_BIT ((u64)1 << 63)
