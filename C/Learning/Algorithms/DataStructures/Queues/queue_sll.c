#include <stdio.h>
#include <stdlib.h>

typedef int queueItem;
typedef struct NodeQ {
  queueItem item;
  struct NodeQ *next;
} NodeQ;

typedef struct Queue {
  int size;
  NodeQ* front;
  NodeQ* rear;
} Queue;

Queue * createQueue() {
  Queue *temp = malloc(sizeof(Queue));
  temp->size = 0;
  temp->front = NULL;
  temp->rear = NULL;
  return temp;
}

void enqueue(Queue *q, int x) {
  NodeQ* temp = (NodeQ*)malloc(sizeof(NodeQ));
  temp->item = x;
  temp->next = NULL;
  if(q->front == NULL && q->rear == NULL) {
    q->front = q->rear = temp;
    q->size = 1;
    return;
  }
  (q->rear)->next = temp;
  q->rear = temp;
  q->size++;
}

queueItem dequeue(Queue *q) {
  NodeQ *temp = q->front;
  if(q->front == NULL) {
    printf("Queue is empty\n.");
    return 0;
  }
  if(q->front == q->rear) {
    q->front = q->rear = NULL;
  } else {
    q->front = q->front->next;
  }
  queueItem dequeued = temp->item;
  free(temp);
  q->size--;
  return dequeued;
}

void freeQueue(Queue *q) {
  NodeQ *temp = q->front;
  while(temp != NULL) {
    free(temp);
    temp = temp->next;
  }
  q->front = NULL;
  q->rear = NULL;
}

void printQueue(Queue *q) {
  NodeQ *temp = q->front;
  printf("Queue: ");
  while(temp != NULL) {
    printf("%d ", temp->item);
    temp = temp->next;
  }
  printf("\n");
}

queueItem frontQueue(Queue *q) {
  if(q->front == NULL) {
    printf("Queue is empty.\n");
    return 0;
  }
  return (q->front)->item;
}

queueItem rearQueue(Queue *q) {
  if(q->front == NULL) {
    printf("Queue is empty.\n");
    return 0;
  }
  return (q->rear)->item;
}

int emptyQueue(Queue *q) {
  if(q->front == NULL) {
    return 1;
  }
  return 0;
}

/* int main() { */
/*   Queue *numQ = createQueue(); */
/*   enqueue(numQ, 10); */
/*   printQueue(numQ); */
/*   enqueue(numQ, 20); */
/*   printQueue(numQ); */
/*   enqueue(numQ, 30); */
/*   printQueue(numQ); */
/*   printf("Front: %d, Rear: %d\n", frontQueue(numQ), rearQueue(numQ)); */
/*   dequeue(numQ); */
/*   printQueue(numQ); */
/*   dequeue(numQ); */
/*   printQueue(numQ); */
/*   printf("Front: %d, Rear: %d\n", frontQueue(numQ), rearQueue(numQ)); */
/*   enqueue(numQ, 40); */
/*   printQueue(numQ); */
/*   printf("Front: %d, Rear: %d\n", frontQueue(numQ), rearQueue(numQ)); */
/*   enqueue(numQ, 50); */
/*   printQueue(numQ); */
/*   freeQueue(numQ); */
/*   free(numQ); */
/*   return 0; */
/* } */
