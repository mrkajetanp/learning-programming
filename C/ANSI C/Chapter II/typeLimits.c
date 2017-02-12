#include <stdio.h>
#include <limits.h>
#include <float.h>

int main () {
  printf("Char bits: %d\n", CHAR_BIT);
  printf("Char: (%d) - %d\n", CHAR_MIN, CHAR_MAX);
  printf("Unsigned Char: %d - %d\n", 0, UCHAR_MAX);
  printf("Signed Char: (%d) - %d\n", SCHAR_MIN, SCHAR_MAX);
  printf("Int: (%d) - %d\n", INT_MIN, INT_MAX);
  printf("Unsigned Int: %d - %u\n", 0, UINT_MAX);
  printf("Long: (%ld) - %ld\n", LONG_MIN, LONG_MAX);
  printf("Unsigned Long: %d - %lu\n", 0, ULONG_MAX);
  printf("Short: (%d) - %d\n", SHRT_MIN, SHRT_MAX);
  printf("Unsigned Short: %d - %d\n", 0, USHRT_MAX);
  printf("\n\n");
  printf("Float: %f - %f\n", FLT_MIN, FLT_MAX);
  printf("Double: %f - %f\n", DBL_MIN, DBL_MAX);

  return 0;
}
