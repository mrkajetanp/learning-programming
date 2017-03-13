#ifndef SINGLE_LINKED_LIST_H
#define SINGLE_LINKED_LIST_H

typedef void(*DISPLAY)(void*);
typedef int(*COMPARE)(void*, void*);

typedef struct _sListNode {
    void *item;
    struct _sListNode *next;
} SLinkedListNode;

typedef struct _SLinkedList {
    SLinkedListNode *head;
    SLinkedListNode *tail;
    SLinkedListNode *current;
    int nItems;
} SLinkedList;

void sLinkedListInitialize(SLinkedList*);
void sLinkedListAddHead(SLinkedList*, void*);
void sLinkedListAddTail(SLinkedList*, void*);
void sLinkedListDelete(SLinkedList*, SLinkedListNode*);
void sLinkedListDisplay(SLinkedList*, DISPLAY);
SLinkedListNode *sLinkedListGetNode(SLinkedList*, COMPARE, void*);

#endif
