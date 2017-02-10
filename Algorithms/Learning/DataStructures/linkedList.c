#include <stdio.h>
#include <stdlib.h>

typedef int listItem;
typedef struct List {
    listItem item;
    struct List *next;
} List;

List *initList (listItem headItem) {
    List *temp = malloc(sizeof(List));
    temp->item = headItem;
    temp->next = NULL;
    return temp;
}

void insertList (List **l, listItem newItem) {
    List *temp = malloc(sizeof(List));
    temp->item = newItem;
    temp->next = *l;
    *l = temp;
}

List *searchList (List *l, listItem x) {
  if (l == NULL)  return (NULL);
  if (l->item == x)
    return(l);
  else
    return (searchList(l->next, x));
}

List *predecessorList (List *l, listItem x) {
  if (l == NULL || l->next == NULL)  return (NULL);
  if (l->next->item == x)
    return (l);
  else
    return (predecessorList(l->next, x));
}

void deleteList (List **l, listItem x) {
  List *p = searchList(*l, x);
  if (p != NULL) {
    List *pred = predecessorList(*l, x);
    if (pred == NULL)
      *l = p->next;
    else
      pred->next = p->next;
    free(p);
  }
}

void printList (List *head) {
    List *node = head;
    while (node != NULL) {
        printf("%d ", node->item);
        node = node->next;
    }
    printf("\n");
}

int main () {
    List *start = initList(10);
    printList(start);
    insertList(&start, 5);
    printList(start);
    insertList(&start, 15);
    insertList(&start, 25);
    insertList(&start, 71);
    printList(start);
    printf("%d\n", searchList(start, 5)->next->item);
    deleteList(&start, 15);
    printList(start);
    deleteList(&start, 25);
    printList(start);
    deleteList(&start, 71);
    printList(start);
    deleteList(&start, 10);
    printList(start);
    return 0;
}
