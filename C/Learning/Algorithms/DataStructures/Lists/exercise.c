#include <stdio.h>
#include <stdlib.h>

typedef int listItem;
typedef struct List {
    listItem item;
    struct List *next;
} List;

List *initList (listItem);
void insertList (List**, listItem);
void printList (List*);

List* initList (listItem x) {
    List *p = malloc(sizeof(List));
    p->item = x;
    p->next = NULL;
    return p;
}

void insertList (List **l, listItem x) {
    List *p = malloc(sizeof(List));
    p->item = x;
    p->next = *l;
    *l = p;
}

List *searchList (List *l, listItem x) {
    if (l == NULL)  return (NULL);
    if (l->item == x)
        return (l);
    else
        return (searchList (l->next, x));

}

List *predecessorList (List *l, listItem x) {
    if (l == NULL || l->next == NULL)   return (NULL);
    if (l->next->item == x)
        return (l);
    else
        return (predecessorList(l->next, x));
}

void deleteList (List **l, listItem x) {
    List *p = searchList (*l, x);
    if (p != NULL) {
        List *pred = predecessorList (*l, x);
        if (pred == NULL)
            *l = p->next;
        else
            pred->next = p->next;
        free (p);
    }
}

void printList (List *start) {
    List *node = start;
    while (node != NULL) {
        printf ("%d ", node->item);
        node = node->next;
    }
    printf ("\n");
}

int main () {
    List *start = initList(10);
    insertList (&start, 15);
    insertList (&start, 35);
    printList (start);
    printf ("Next to 15: %d\n", searchList(start, 15)->next->item);
    printf ("Predecessor of 15: %d\n", predecessorList(start, 15)->item);
    deleteList (&start, 15);
    printList (start);
    insertList (&start, 22);
    printList (start);

    return 0;
}
