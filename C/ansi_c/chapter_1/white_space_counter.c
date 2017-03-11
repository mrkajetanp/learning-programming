#include <stdio.h>

int main () {
  int c, nls, spaces, tabs;
  nls = spaces = tabs = 0;
  while ((c = getchar()) != EOF)
    if (c == '\n')
      ++nls;
    else if (c == '\t')
      ++tabs;
    else if (c == ' ')
      ++spaces;
    printf("Lines: %d\nSpaces: %d\nTabs: %d\n", nls, spaces, tabs);

  return 0;
}
