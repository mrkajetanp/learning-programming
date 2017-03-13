/* negating the number */

#include <stdio.h>

int main() {
  int a = 10;
  printf("Positive %d\n", a);
  a = -a;
  printf("Negated %d\n", a);
  a = -a;
  printf("Positive %d\n", a);
  a *= -1;
  printf("Negated %d\n", a);

  return 0;
}
