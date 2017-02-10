#include <stdio.h>

typedef int bool;
#define true 1
#define false 0

int main () {
  int c, brackets = 0, parenthesis = 0, squarebr = 0, dquotes = 0, quotes = 0, comments = 0;
  bool ifSlash = false, ifHalfComment = false;
  while ((c = getchar()) != EOF) {
    if (c == '\\')
      ifSlash = true;
    else if (c == '/' && !ifHalfComment)
      ifHalfComment = true;
    else if (c == '{')
      ++brackets;
    else if (c == '}')
      --brackets;
    else if (c == '(')
      ++parenthesis;
    else if (c == ')')
      --parenthesis;
    else if (c == '[')
      ++squarebr;
    else if (c == ']')
      --squarebr;
    else if (c == '"') {
      if (!ifSlash)
        ++dquotes;
    }
    else if (c == '\'') {
      if (!ifSlash)
        ++quotes;
    }
    else if (c == '*' && ifHalfComment) {
      ++comments;
      ifHalfComment = false;
    }
    else if (c == '*' && !ifHalfComment)
      ifHalfComment = true;
    else if (c == '/' && ifHalfComment) {
      --comments;
      ifHalfComment = false;
    }
    else if (ifHalfComment)
      ifHalfComment = false;
    if (ifSlash && c != '\\')
      ifSlash = false;
  }
  if (brackets)
    printf("Brackets not matching!\n");
  if (parenthesis)
    printf("parenthesis not matching!\n");
  if (squarebr)
    printf("Square Brackets not matching!\n");
  if (dquotes%2 != 0)
    printf("Double quotes not matching!\n");
  if (quotes%2 != 0)
    printf("Single quotes not matching!\n");
  if (comments)
    printf("Comments not matching!\n");
  if (!brackets && !parenthesis && !squarebr && dquotes%2 == 0 && quotes%2 == 0 && !comments)
    printf("Everything's fine!\n");

  return 0;
}
