#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>

jmp_buf test1;

void tryjump() {
  longjmp(test1, 3);
}

static void catch_function(int signal) {
  puts("Interactive attention signal caught.");
}

int main() {
  printf("test\n\n");
  /* assert */

  char *ptr = malloc(20UL); // allocating memory

  if(ptr == NULL) {
    perror("malloc failed");
  } else {
    free(ptr); // freeing memory
  }

  /* preventing divide by zero errors */

  int divident = 50;
  int divisor = 0;
  int quotient;

  if(divisor == 0) {
    /* handling the error in here */
    fprintf(stderr, "Division by zero! Aborting...\n");
    /* exit(EXIT_FAILURE); */
  } else {
    quotient = divident / divisor;
    printf("%d\n", quotient);
    /* exit(EXIT_SUCCESS); */
  }

  /* handling signals */

  if(signal(SIGINT, catch_function) == SIG_ERR) {
    fputs("An error occurred while setting a signal handler.\n", stderr);
    return EXIT_FAILURE;
  }
  puts("Raising the interactive attention signal.");
  if(raise(SIGINT) != 0) {
    fputs("Error raising the signal.\n", stderr);
    return EXIT_FAILURE;
  }
  puts("Exiting.");

  /* setjmp */

  if(setjmp(test1) == 0) {
    printf("setjmp() returned 0.\n");
    tryjump();
  } else {
    printf("setjmp returned from a longjmp function call\n");
  }





  return 0;
}













