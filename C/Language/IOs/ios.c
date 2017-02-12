#include <stdio.h>

int main() {
  printf("\n");
  int x = 0;
  printf("Input: ");
  scanf("%d", &x); /* getting input from the user and saving it to x */
  /* but \n remains in standard out */
  printf("Input was: %d\n", x);
  getchar(); /* getchar to clear \n from the stream */

  printf("GetChar: ");
  /* int x2 = getchar(); /\* returns an int which is either EOF or or the next character in std input stream *\/ */
  int x2;
  x2 = getchar();
  printf("Got char: ");
  putchar(x2);

  
  return 0;
}
