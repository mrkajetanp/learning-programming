#ifndef STACK_H
#define STACK_H

#include "SingleLinkedList.h"

typedef SLinkedList Stack;

void stackInitialize(Stack*);
void stackPush(Stack*, void*);
void* stackPop(Stack*);

#endif
