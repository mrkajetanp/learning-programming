#include <stdio.h>
#include <stdlib.h>

typedef int listItem;
typedef struct List {
  listItem item;       // data item
  struct List *next;   // ptr to successor
  struct List *prev;   // ptr to predecessor
} List;

List *search_list(List *l, listItem x) {
  if(l == NULL) return(NULL);
  if(l->item == x)
    return(l);
  else
    return(search_list(l->next, x));
}

void insert_list(List **l, listItem x) {
  List *p;                    // temporary pointer
  List *tempStart = *l;
  p = malloc(sizeof(List));   //
  p->item = x;                // l - variable maintaining access to the head of the list
  p->next = *l;               // just have to update pointer
  *l = p;                     // pointer to the head of the list
  tempStart->prev = *l;
}

void delete_list(List **l, listItem x) {
  List *p, *pred;
  List *search_list();

  p = search_list(*l, x);
  if(p != NULL) {
    pred = p->prev;
    if(pred == NULL) {
      (p->next)->prev = NULL;
      *l = p->next;
    } else {
      pred->next = p->next;
      (p->next)->prev = pred;
    }
    free(p);
  }
}

void print_list(List *start) {
  List *node = start;
  while(node != NULL) {
    printf("%d ", node->item);
    node = node->next;
  }
  printf("\n");
}

int main() {
  List *listOneStart = malloc(sizeof(List));
  listOneStart->item = 5;

  insert_list(&listOneStart, 10);
  insert_list(&listOneStart, 20);
  insert_list(&listOneStart, 30);
  print_list(listOneStart);

  List *listOneSecond = listOneStart->next;

  printf("First item: %d\n", listOneStart->item);
  printf("Second item: %d\n", listOneSecond->item);
  printf("Predecessor of %d: %d\n", listOneSecond->item, (listOneSecond->prev)->item);
  printf("Predecessor of 10: %d\n", (search_list(listOneStart, 10)->prev)->item);
  printf("Item next to searched node (20): %d\n", (search_list(listOneStart, 20)->next)->item);

  delete_list(&listOneStart, 20);
  print_list(listOneStart);

  insert_list(&listOneStart, 15);
  insert_list(&listOneStart, 10);
  insert_list(&listOneStart, 25);
  print_list(listOneStart);
  /* printf("Predecessor of 10: %d\n", ) */

  free(listOneStart);
  return 0;
}



