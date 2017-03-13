#include <stdio.h>

#define MAXLINE 1000
#define true 1
#define false 0
typedef int bool;

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

void removeComments (char s[], int len) {
    bool wasHalfComment = false, isComment = false, isInQuote = false;
    for (int i = 0 ; i < len ; ++i) {
        if (!isInQuote && s[i] == '"') {
            isInQuote = true;
            continue;
        }
        if (isInQuote && s[i] == '"') {
            isInQuote = false;
            continue;
        }
        if (isInQuote)
            continue;
        if (!isComment) {
            if (s[i] == '/')
                wasHalfComment = true;
            else if (wasHalfComment && s[i] == '*') {
                s[i-1] = ' ';
                s[i] = ' ';
                wasHalfComment = false;
                isComment = true;
            }
        } else {
            if (s[i] == '*')
                wasHalfComment = true;
            else if (wasHalfComment && s[i] == '/') {
                s[i-1] = ' ';
                s[i] = ' ';
                isComment = false;
            }
            else {
                s[i] = ' ';
            }
        }
    }
}

int main () {
    int len;
    char line[MAXLINE];
    while ((len = getLine(line, MAXLINE)) > 0) {
        removeComments(line, len);
        printf("%s", line);
    }
    return 0;
}
