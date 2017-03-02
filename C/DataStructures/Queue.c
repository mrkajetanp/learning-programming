#include "Queue.h"
#include <stdlib.h>

void queueInitialize(Queue* queue) {
    sLinkedListInitialize(queue);
}

void queueEnqueue(Queue* queue, void* node) {
    sLinkedListAddHead(queue, node);
}

void queueDequeue(Queue* queue) {
    SLinkedListNode* tmp = queue->head;
    void *item;

    if (queue->head == NULL)
        item = NULL;
    else if (queue->head == queue->tail) {
        queue->head = queue->tail = NULL;
        item = tmp->item;
        free(tmp);
    }
    else {
        while (tmp->next != queue->tail)
            tmp = tmp->next;

        queue->tail = tmp;
        tmp = tmp->next;
        queue->tail->next = NULL;
        item = tmp->item;
        free(tmp);
    }
}
