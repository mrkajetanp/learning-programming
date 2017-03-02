#include "Stack.h"
#include <stdlib.h>

void stackInitialize(Stack* stack) {
    sLinkedListInitialize(stack);
}

void stackPush(Stack* stack, void* item) {
    sLinkedListAddHead(stack, item);
}

void* stackPop(Stack* stack) {
    SLinkedListNode* node = stack->head;
    if (node == NULL)
        return NULL;
    else if (node == stack->tail) {
        stack->head = stack->tail = NULL;
        void* item = node->item;
        free(node);
        return item;
    }
    else {
        stack->head = stack->head->next;
        void* item = node->item;
        free(node);
        return item;
    }
}


