#ifndef QUEUE_H
#define QUEUE_H

#include "SingleLinkedList.h"

typedef SLinkedList Queue;

void queueInitialize(Queue*);
void queueEnqueue(Queue* queue, void* node);
void queueDequeue(Queue* queue);

#endif
