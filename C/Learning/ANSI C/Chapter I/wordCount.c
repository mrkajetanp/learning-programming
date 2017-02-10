#include <stdio.h>

#define IN 1
#define OUT 0

int main () {
  int c, nl = 0, nw = 0, nc = 0, state = OUT;
  while ((c = getchar()) != EOF) {
    ++nc;
    if (c == '\n')
      ++nl;
    if (c == ' ' || c == '\n' || c == '\t')
      state = OUT;
    else if (state == OUT) {
      state = IN;
      ++nw;
    }
  }
  printf("%d %d %d\n", nl, nw, nc);

  return 0;
}
