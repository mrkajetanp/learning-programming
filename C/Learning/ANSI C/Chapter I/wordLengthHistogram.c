#include <stdio.h>

#define IN 1
#define OUT 0

int main () {
    int wordLengths[10];
    int c, currentWord = 0, state = OUT;
    for (int i = 0 ; i < 10 ; ++i)
        wordLengths[i] = 0;
    while ((c = getchar()) != EOF) {
        if (c == ' ' || c == '\n' || c == '\t') {
          state = OUT;
          ++currentWord;
        }
        else if (state == OUT) {
          state = IN;
        }
        if (c != ' ')
            ++wordLengths[currentWord];
    }
    for (int i = 0 ; i < currentWord ; ++i) {
        for (int j = 0 ; j < wordLengths[i] ; ++j)
            printf("=");
        printf("\n");
    }

    return 0;
