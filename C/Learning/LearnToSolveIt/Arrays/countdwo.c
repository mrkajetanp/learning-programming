#include <stdio.h>

/* count digits, whitespaces and others */

int main() {

  int c, i, nwhite = 0, nother = 0;
  int ndigits[10];

  for(i = 0 ; i < 10 ; ++i)
    ndigits[i] = 0;

  while((c = getchar()) != EOF)
    if(c >= '0' && c <= '9')
      ++ndigits[c-'0'];
    else if (c == ' ' || c == '\n' || c == '\t')
      ++nwhite;
    else
      ++nother;

  printf("digits = ");
  for(i = 0 ; i < 10 ; ++i)
    printf("%d - %d ; ", i, ndigits[i]);

  printf(", whitespace - %d, others = %d\n", nwhite, nother);

  return 0;
}




