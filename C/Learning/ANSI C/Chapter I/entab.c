#include <stdio.h>

#define INDENT 4

int main () {
    int c, wsl = 0;
    while ((c = getchar()) != EOF) {
        if (c != ' ') {
            for (int i = 0 ; i < wsl/INDENT ; ++i)
                putchar('\t');
            for (int i = 0 ; i < wsl%INDENT ; ++i)
                putchar(' ');
            wsl = 0;
            putchar(c);
        }
        else {
            ++wsl;
        }
    }
    return 0;
}
