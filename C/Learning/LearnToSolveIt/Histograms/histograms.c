#include <stdio.h>
#include <stdlib.h>

void flushinput() {
  int c;
  while((c = getchar()) != '\n' && c != EOF)
    ;
}

int main() {

  { // ownspace+

  int c;
  while((c = getchar()) != EOF) {
    if(c == ' ' || c == '\n' || c == '\t')
      putchar('\n');
    else
      putchar('*');
  }

  } // ownspace-

  puts("\n");

  /* vertical histogram of words in a sentence */

  int i, c, j, nc, nw;

  int maxno = 0;
  int maxwl = 0;
  printf("Maximum number of words: ");
  scanf("%d", &maxno);
  printf("Maximum number of character in words: ");
  scanf("%d", &maxwl);
  printf("%d %d\n", maxno, maxwl);

  flushinput();

  int *word = malloc(sizeof(int[maxno]));

  for(i = 0 ; i < maxno ; ++i)
    word[i] = 0;

  nc = nw = 0;

  while((c = getchar()) != EOF) {
    ++nc;
    if(c == ' ' || c == '\n' || c == '\t') {
      word[nw] = nc -1; // -1 for excluing the space in the word length
      ++nw;
      nc = 0; // resetting the word-length for the next word
    }
  }

  for(i = 0 ; i < maxno ; ++i)
    printf("%d ", word[i]);
  printf("\n");

  for(i = maxwl ; i >= 1; --i) {
    for(j = 0 ; j <= nw ; ++j) {
      if(i <= word[j])
        printf("*  ");
      else
        printf("   ");
    }
    putchar('\n');
  }

  /* freeing allocated memory+ */
  free(word);
  /* freeing allocated memory- */

  return 0;
}
