#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <wchar.h>
#include <limits.h>
#include <float.h>
#include <math.h>

int main() {
  /* size_t - unsigned integer type returned by the sizeof operator */
  /* ptrdiff_t - signed integer type returned when substracting two pointers */
  /* NULL - null pointer constant */
  /* max_align_t - a type with alignment requirement as great as other scalar type */
  /* offsetof - byte offset from the beginning of a struct type to specified member // stdalign.h */
  /* alignas ; alignof (stdnoreturn.h) */
  /* noreturn */

  bool a = true, b = false;
  printf("%d %d\n", a, b);

  /* fixed width integer types - stdint */

  /* int8_t ; int16_t ; int32_t ; int64_t - signed integer types with widths in bits */
  /* int_fast8_t ; int_fast16_t ; int_fast32_t ; int_fast64_t - fastest signed integer type with width of the least ... */
  /* int_least8_t ; int_least16_t ; int_least32_t ; int_least64_t - smallest signed integer type with width of at least 8 etc. ... */
  /* intmax_t - maximum width integer type ; intptr_t - integer type capable of holding a pointer */
  /* uint8_t ; uint16_t .. uint64_t - unsigned integer type with width of exactly 8, 16, ... */
  /* uint_fast8_t ... uint_fast64_t - fastest unsigned integer type with width of ... */
  /* uint_least8_t ; uint_least64_t - smallest unsigned ... */
  /* uintmax_t - maximum width unsigned ... */
  /* uintptr_t - unsigned integer type capable of holding a pointer  */

  printf("PTRDIFF_MIN    = %td\n", PTRDIFF_MIN);
  printf("PTRDIFF_MAX    = %+td\n", PTRDIFF_MAX);
  printf("SIZE_MAX       = %zu\n", SIZE_MAX);
  printf("SIG_ATOMIC_MIN = %+jd\n",(intmax_t)SIG_ATOMIC_MIN);
  printf("SIG_ATOMIC_MAX = %+jd\n",(intmax_t)SIG_ATOMIC_MAX);
  printf("WCHAR_MIN      = %+jd\n",(intmax_t)WCHAR_MIN);
  printf("WCHAR_MAX      = %+jd\n",(intmax_t)WCHAR_MAX);
  printf("WINT_MIN       = %jd\n", (intmax_t)WINT_MIN);
  printf("WINT_MAX       = %jd\n", (intmax_t)WINT_MAX);
  printf("\n");

  printf("CHAR_BIT   = %d\n", CHAR_BIT);
  printf("MB_LEN_MAX = %d\n", MB_LEN_MAX);
  printf("\n");

  printf("CHAR_MIN   = %+d\n", CHAR_MIN);
  printf("CHAR_MAX   = %+d\n", CHAR_MAX);
  printf("SCHAR_MIN  = %+d\n", SCHAR_MIN);
  printf("SCHAR_MAX  = %+d\n", SCHAR_MAX);
  printf("UCHAR_MAX  = %u\n",  UCHAR_MAX);
  printf("\n");

  printf("SHRT_MIN   = %+d\n", SHRT_MIN);
  printf("SHRT_MAX   = %+d\n", SHRT_MAX);
  printf("USHRT_MAX  = %u\n",  USHRT_MAX);
  printf("\n");

  printf("INT_MIN    = %+d\n", INT_MIN);
  printf("INT_MAX    = %+d\n", INT_MAX);
  printf("UINT_MAX   = %u\n",  UINT_MAX);
  printf("\n");

  printf("LONG_MIN   = %+ld\n", LONG_MIN);
  printf("LONG_MAX   = %+ld\n", LONG_MAX);
  printf("ULONG_MAX  = %lu\n",  ULONG_MAX);
  printf("\n");

  printf("LLONG_MIN  = %+lld\n", LLONG_MIN);
  printf("LLONG_MAX  = %+lld\n", LLONG_MAX);
  printf("ULLONG_MAX = %llu\n",  ULLONG_MAX);
  printf("\n");

  printf("FLT_RADIX    = %d\n", FLT_RADIX);
  printf("DECIMAL_DIG  = %d\n", DECIMAL_DIG);
  printf("FLT_MIN      = %e\n", FLT_MIN);
  printf("FLT_MAX      = %e\n", FLT_MAX);
  printf("FLT_EPSILON  = %e\n", FLT_EPSILON);
  printf("FLT_DIG      = %d\n", FLT_DIG);
  printf("FLT_MANT_DIG = %d\n", FLT_MANT_DIG);
  printf("FLT_MIN_EXP  = %d\n",  FLT_MIN_EXP);
  printf("FLT_MIN_10_EXP  = %d\n",  FLT_MIN_10_EXP);
  printf("FLT_MAX_EXP     = %d\n",  FLT_MAX_EXP);
  printf("FLT_MAX_10_EXP  = %d\n",  FLT_MAX_10_EXP);
  printf("FLT_ROUNDS      = %d\n",  FLT_ROUNDS);
  printf("FLT_EVAL_METHOD = %d\n",  FLT_EVAL_METHOD);
  printf("FLT_HAS_SUBNORM = %d\n",  FLT_HAS_SUBNORM);

  return 0;
}
