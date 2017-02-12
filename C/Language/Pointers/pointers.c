/* #include <assert.h> /\* assertion *\/ */
/* #include <complex.h> /\* complex number arithmetics *\/ */
/* #include <ctype.h> /\* functions to determine the type contained in character data *\/ */
/* #include <errno.h> /\* reporting error conditions *\/ */
/* #include <fenv.h> /\* floating-point environment *\/ */
/* #include <float.h> /\* limits of float types *\/ */
/* #include <inttypes.h> /\* format conversion of integer types *\/ */
/* #include <iso646.h> /\* alternative operator spellings *\/ */
/* #include <limits.h> /\* sizes of basic types *\/ */
/* #include <locale.h> /\* localization utilities *\/ */
/* #include <math.h> /\* common mathematics functions *\/ */
/* #include <setjmp.h> /\* nonlogical jumps *\/ */
/* #include <signal.h> /\* signal handling *\/ */
/* #include <stdalign.h> /\* alignas and alignof convenience macros *\/ */
/* #include <stdarg.h> /\* variable arguments *\/ */
/* #include <stdatomic.h> /\* atomic types *\/ */
/* #include <stdbool.h> /\* boolean type *\/ */
/* #include <stddef.h> /\* common macro definitions *\/ */
/* #include <stdint.h> /\* fixed-width integer types *\/ */
#include <stdio.h> /* input / output */
/* #include <stdlib.h> /\* general utilities: memory management, program utilities, string conversions, random numbers *\/ */
/* #include <stdnoreturn.h> /\* noreturn convenience macros *\/ */
/* #include <string.h> /\* string handling *\/ */
/* #include <tgmath.h> /\* type-generic math (macros wrapping math.h and complex.h) *\/ */
/* #include <pthread.h> /\* threads *\/ */
/* #include <time.h> /\* time/date utilities *\/ */
/* #include <uchar.h> /\* UTF-16 and UTF-32 char utils *\/ */
/* #include <wchar.h> /\* extended multibyte and wide character utilities *\/ */
/* #include <wctype.h> /\* wide character classification and mapping utilities *\/ */

int funct(int a, int b) {
  printf("a = %d, b = %d\n", a, b);
  return 0;
}

int main() {

  char a = 'a';
  char *aptr = &a;
  char **aptrp = &aptr;

  printf("a: %c\n", a);
  printf("pointer to a: %d\n", aptr);
  printf("value of pointer to a: %c\n", *aptr);
  printf("pointer to pointer to a: %d\n", aptrp);
  printf("value where pointer to pointer points: %d\n", *aptrp);
  printf("value ...: %c\n", **aptrp);

  /* Array of pointers */

  char *p1 = "Thomas";
  char *p2 = "James";
  char *p3 = "John";

  char *arr[3];
  arr[0] = p1;
  arr[1] = p2;
  arr[2] = p3;
  printf("%s %s %s\n", arr[0], arr[1], arr[2]);

  /* pointers to functions  */

  int(*fptr)(int,int) = funct;

  funct(2,3);
  fptr(2,3);

  enum colors { RED, YELLOW, GREEN=12, BLUE};
  enum colors t = BLUE;
  printf("%d\n", t);

  return 0;
}





