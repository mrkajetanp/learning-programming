#include <stdio.h>

int main () {
  int c, wasSpace = 0;
  while ((c = getchar()) != EOF) {
    if (c != ' ') {
      putchar(c);
      wasSpace = 0;
    }
    else if (wasSpace == 0) {
      putchar(c);
      wasSpace = 1;
    }
  }

  return 0;
}
