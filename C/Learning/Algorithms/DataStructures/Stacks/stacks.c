#include <stdio.h>
#include <stdlib.h>

typedef int stackElement;
typedef struct {
  stackElement *contents;
  int top;
  int maxSize;
} Stack;

void stackInit(Stack *stack, int maxSize) {
  stackElement *newContents;
  // allocate new array to hold the contents

  newContents = (stackElement *)malloc(sizeof(stackElement) * maxSize);
  if(newContents == NULL) {
    fprintf(stderr, "Not enough memory to initialize the stack.\n");
    exit(1); // exit with error code
  }

  stack->contents = newContents;
  stack->maxSize = maxSize;
  stack->top = -1; // empty stack
}

void stackDestroy(Stack *stack) {
  // clean after the array
  free(stack->contents);

  stack->contents = NULL;
  stack->maxSize = 0;
  stack->top = -1; // empty
}

int stackIsEmpty(Stack *stack) {
  return stack->top < 0;
}

int stackIsFull(Stack *stack) {
  return stack->top >= stack->maxSize-1;
}

void stackPush(Stack *stack, stackElement element) {
  if(stackIsFull(stack)) {
    fprintf(stderr, "Can't push the element. Stack is full.\n");
    exit(1);
  }

  /* putting information in array and updating top  */
  stack->contents[++stack->top] = element;
}

stackElement stackPop(Stack *stack) {
  if(stackIsEmpty(stack)) {
    fprintf(stderr, "Can't pop the element from stack. Stack is empty.\n");
    exit(1);
  }

  return stack->contents[stack->top--];
}

void stackPrint(Stack *stack) {
  int elements = stack->top+1;
  printf("Contents of the stack: ");

  if(elements == 0) {
    printf("Stack is empty!\n");
    return;
  }

  for (int i = 0 ; i < elements ; i++) {
    printf("%d ", stack->contents[i]);
  }
  printf("\n");
}

int main() {
  Stack stackInt;
  stackInit(&stackInt, 15);
  int *nums = malloc(sizeof(int[10]));

  {
    for (int i = 0 ; i < 10 ; i++) {
    nums[i] = i;
    printf("%d ", nums[i]);
  }
    puts("");
  } // filling out the array

  stackPush(&stackInt, nums[5]);
  stackPush(&stackInt, nums[6]);
  stackPush(&stackInt, nums[3]);

  stackPrint(&stackInt);

  printf("Popping: %d\n", stackPop(&stackInt));
  stackPrint(&stackInt);
  printf("Popping: %d\n", stackPop(&stackInt));
  stackPrint(&stackInt);

  stackPush(&stackInt, nums[8]);
  stackPrint(&stackInt);

  printf("Popping: %d\n", stackPop(&stackInt));
  stackPrint(&stackInt);

  printf("Popping: %d\n", stackPop(&stackInt));
  stackPrint(&stackInt);

  stackDestroy(&stackInt);
  return 0;
}
