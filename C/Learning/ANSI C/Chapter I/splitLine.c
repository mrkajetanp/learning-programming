#include <stdio.h>

#define MAXLINE 1000
#define SPLITL 80

int getLine (char s[], int lim) {
    int c, i;
    for (i = 0 ; i < lim-1 && (c = getchar()) != EOF && c != '\n' ; ++i)
        s[i] = c;
    if (c == '\n') {
        s[i] = c;
        ++i;
    }
    s[i] = '\0';
    return i;
}

int main () {
    int len, partsNum, splitPos;
    char line[MAXLINE];
    while ((len = getLine(line, MAXLINE)) > 0) {
        if (len > SPLITL) {
            splitPos = SPLITL;
            for (int i = 0 ; i < splitPos ; ++i)
                putchar(line[i]);
            putchar('\n');
            for (int i = splitPos ; i < len ; ++i)
                putchar(line[i]);
        } else {
            printf("%s", line);
        }
    }
    return 0;
}
