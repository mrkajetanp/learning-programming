#include "SingleLinkedList.h"
#include <stdio.h>
#include <stdlib.h>

void sLinkedListInitialize(SLinkedList* list) {
    list->head = NULL;
    list->tail = NULL;
    list->current = NULL;
    list->nItems = 0;
}

void sLinkedListAddHead(SLinkedList* list, void* item) {
    SLinkedListNode* node = malloc(sizeof(SLinkedListNode));
    node->item = item;
    if (list->head == NULL) {
        list->tail = node;
        node->next = NULL;
    } else
        node->next = list->head;
    list->head = node;
}

void sLinkedListAddTail(SLinkedList* list, void* item) {
    SLinkedListNode* node = malloc(sizeof(SLinkedListNode));
    node->item = item;
    node->next = NULL;
    if (list->head == NULL)
        list->head = node;
    else
        list->tail->next = node;
    list->tail = node;
}

void sLinkedListDelete(SLinkedList* list, SLinkedListNode* node) {
    if (node == list->head) {
        if (list->head->next == NULL)
            list->head = list->tail = NULL;
        else
            list->head = list->head->next;
    }
    else {
        SLinkedListNode *tmp = list->head;
        while (tmp != NULL && tmp->next != node)
            tmp = tmp->next;
        if (tmp != NULL)
            tmp->next = node->next;
    }
    free(node);
}

void sLinkedListDisplay(SLinkedList* list, DISPLAY display) {
    printf("\nLinked List\n");
    SLinkedListNode* current = list->head;
    while (current != NULL) {
        display(current->item);
        current = current->next;
    }
}

SLinkedListNode* sLinkedListGetNode(SLinkedList* list, COMPARE compare, void* item) {
    SLinkedListNode* node = list->head;
    while (node != NULL) {
        if (compare(node->item, item) == 0)
            return node;
        node = node->next;
    }
    return NULL;
}
