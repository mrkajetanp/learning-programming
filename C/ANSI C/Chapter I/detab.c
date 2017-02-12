#include <stdio.h>

#define INDENT 4

int main () {
    int c;
    while ((c = getchar()) != EOF) {
        if (c == '\t')
            for (int i = 0 ; i < INDENT ; i++)
                putchar(' ');
        else
            putchar(c);
    }

    return 0;
}
