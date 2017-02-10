#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>

/* exception handling + */
#define TRY do { jmp_buf ex_buf__; switch( setjmp(ex_buf__)) { case 0: while(1) {
#define CATCH(x) break; case x:
#define FINALLY break; } default: {
#define ETRY break; } } }while(0)
#define THROW(x) longjmp(ex_buf__, x)

#define FOO_EXCEPTION (1)
#define BAR_EXCEPTION (2)
#define BAZ_EXCEPTION (3)
/* exception handling - */

volatile sig_atomic_t gSignalStatus;

void signal_handler(int signal) {
  gSignalStatus = signal;
}

int main() {
  puts("");

  /* dynamic memory management */

  /* malloc - allocates size bytes of uninitialized storage */
  /* void* malloc( size_t size ); */
  /* On success, returns the pointer to the beginning of newly allocated memory. The returned pointer must be deallocated with free() or realloc(). */
    /* On failure, returns a null pointer.  */

  int *p1 = malloc(4*sizeof(int)); // allocates enough for an array of 4 int
  int *p2 = malloc(sizeof(int[4])); // the same, naming the type directly
  int *p3 = malloc(4*sizeof *p3); // same, without repeating the type name

  if(p1) {
    for(int n=0 ; n<4 ; ++n) // filling the array with ints
      p1[n] = n*n;
    for(int n=0 ; n<4 ; ++n) // printing it back out
      printf("p1[%d] == %d\n", n, p1[n]);
  }

  /* freeing memory after mallocs */
  free(p1);
  free(p2);
  free(p3);

  puts("");

  /* calloc - allocates memory for an array of num objects of size size and initializes all bits in the allocated storage to zero ; calloc is thread-safe */
  /* void* calloc( size_t num, size_t size ); - num - number of objects ; size - size of each object */
  /* On success, returns the pointer to the beginning of newly allocated memory. The returned pointer must be deallocated with free() or realloc(). */
    /* On failure, returns a null pointer.  */

  int *pc1 = calloc(4, sizeof(int)); // allocate and zero out an array of 4 int
  int *pc2 = calloc(1, sizeof(int[4])); // same, naming the array type directly
  int *pc3 = calloc(4, sizeof *p3); // same, without repeating the type name

  if(pc2) {
    for(int n = 0 ; n < 4 ; ++n) // print the array
      printf("pc2[%d] == %d\n", n, pc2[n]);
  }

  free(pc1);
  free(pc2);
  free(pc3);

  puts("");

  /* realloc - reallocates the given area of memory. it must be previously allocated by malloc(), calloc() or realloc() and not yet freed with a call to free or realloc */
  /* void *realloc( void *ptr, size_t new_size ); - ptr - pointer to the memory area to be reallocated ; new_size - new size of the array */
  /* On success, returns the pointer to the beginning of newly allocated memory. The returned pointer must be deallocated with free() or realloc(). The original pointer ptr is invalidated and any access to it is undefined behavior (even if reallocation was in-place). */
    /* On failure, returns a null pointer. The original pointer ptr remains valid and may need to be deallocated with free() or realloc().  */

  int *pa = malloc(10 * sizeof *pa); // allocate an array of 10 int
  if(pa) {
    printf("%zu bytes allocated. Storing ints: ", 10*sizeof(int));
    for(int n = 0 ; n < 10 ; ++n)
      printf("%d ", pa[n] = n);
  }

  int *pb = realloc(pa, 15 * sizeof *pb); // reallocate pa to a larger array of 15 ints
  if(pb) {
    printf("\n%zu bytes allocated, first 10 ints are: ", 15*sizeof(int));
    for(int n = 0 ; n < 10 ; ++n)
      printf("%d ", pb[n]); // show the array
    free(pb);
  } else { // if realloc failed, the original pointer needs to be freed
    free(pa);
  }

  puts("");

  /* free - deallocates the space previously allocated by malloc(), calloc(), aligned_alloc() or realloc() */
  /* void free( void* ptr ); - ptr - pointer to the memory to deallocate */
  /* returns nothing */

  /* aligned_alloc - Allocate size bytes of uninitialized storage whose alignment is specified by alignment */
  /* void *aligned_alloc( size_t alignment, size_t size ); - alignment - specifies the alignment. Must be a valid alignment supported by the implementation. ; size 	- 	number of bytes to allocate */
  /* On success, returns the pointer to the beginning of newly allocated memory. The returned pointer must be deallocated with free() or realloc(). */
    /* On failure, returns a null pointer.  */


  int *paa1 = malloc(10*sizeof *paa1);
  printf("default-aligned addr:   %p\n", (void*)paa1);
  free(paa1);

  int *paa2 = aligned_alloc(1024, 10*sizeof *paa2);
  printf("1024-byte aligned addr: %p\n", (void*)paa2);
  free(paa2);

  puts("");


  /* program support utilities */
  /* stdlib ; functions */

  /* abort() - causes abnormal program termination (without cleaning up) */
  /* exit() - causes normal program termination with cleaning up */
  /* quick_exit() - causes normal program termination without completely cleaning up */
  /* _Exit() - causes normal program termination without cleaning up */
  /* atexit(function) - registers a function to be called on exit() invocation */
  /* at_quick_exit(function) - registers a function to be called on quick_exit invocation */
  /* EXIT_SUCCESS ; EXIT_FAILURE - indicates a program execution status ; exit(EXIT_FAILURE) ; return EXIT_SUCCESS */

  /* communicating with the environment */
  /* system(command) ; calls the host environment's command processor with command parameter ; returns implementation-defined value */
  /* int system( const char *command ); */
  system("ls");

  /* getenv, getenv_s - getting an environment variable */
  /* char *getenv( const char *name ); */
  /* errno_t getenv_s( size_t *restrict len, char *restrict value, rsize_t valuesz, const char *restrict name ); */
  /* name 	- 	null-terminated character string identifying the name of the environmental variable to look for */
  /* len 	- 	pointer to a user-provided location where getenv_s will store the length of the environment variable */
  /* value 	- 	pointer to a user-provided character array where getenv_s will store the contents of the environment variable */
  /* valuesz 	- 	maximum number of characters that getenv_s is allowed to write to dest (size of the buffer)  */

  char *env_p = getenv("PATH");
  if(env_p)
    printf("PATH = %s\n", env_p);

  /* signal - sets a signal handler for particular signal */
  /* SIGABRT ; SIGFPE ; SIGILL ; SIGINT ; SIGSEGV ; SIGTERM */
    /* defines signal types */
  /* void (*signal( int sig, void (*handler) (int))) (int); */
  /* raise(signal) runs the signal handler for particular signal */

  signal(SIGINT, signal_handler);

  printf("SignalValue: %d\n", gSignalStatus);
  printf("Sending signal: %d\n", SIGINT);
  raise(SIGINT);
  printf("SignalValue: %d\n", gSignalStatus);

  /* sig_atomic_t - the integer type that can be accessed as an atomic enitity from an asynchronous signal handler */
  /* SIG_DFL ; SIG_IGN - defines signal handling strategies ; SIG_ERR - error was encountered */

  /* signal(SIGTERM, SIG_IGN); // ignoring the signal */
  /* raise(SIGTERM); - term signal will be ignored */
  if(signal(SIGTERM, signal_handler) == SIG_ERR) {
    puts("Error while installing a signal handler.");
    exit(EXIT_FAILURE);
  }

  /* non-local jumps */
  /* setjmp ; longjmp */
  /* jmp_buf - execution context type */
  /* they can be used to handling exceptions - see the top */

  puts("");

  TRY {
    printf("In Try Statement\n");
    THROW( FOO_EXCEPTION );
    printf("I do not appear\n");
  }
  CATCH( FOO_EXCEPTION ) {
    printf("Got Foo!\n");
  }
  CATCH( BAR_EXCEPTION ) {
    printf("Got Bar!\n");
  }
  CATCH( BAZ_EXCEPTION ) {
    printf("Got Baz!\n");
  }
  FINALLY {
    printf("Finalizing...\n");
  }
  ETRY;


  puts("");
  return 0;
}













