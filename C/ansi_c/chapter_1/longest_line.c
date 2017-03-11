#include <stdio.h>

#define MAXLINE 1000

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

void copy (char to[], char from[]) {
    int i = 0;
    while ((to[i] = from[i]) != '\0')
        ++i;
}

int main () {
    int len, max = 0;
    char line[MAXLINE];
    while ((len = getLine(line, MAXLINE)) > 0)
        printf("%d: %s", len-1, line);
    return 0;
}
