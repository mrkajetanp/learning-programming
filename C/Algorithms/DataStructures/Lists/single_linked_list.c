#include <stdio.h>
#include <stdlib.h>

typedef int listItem;
typedef struct List {
  listItem item;       // data item
  struct List *next;   // ptr to successor
} List;

List *search_list(List *l, listItem x) {
  if(l == NULL) return(NULL);
  if(l->item == x)
    return(l);
  else
    return(search_list(l->next, x));
}

void insert_list(List **l, listItem x) {
  List *p;          // temporary pointer
  p = malloc(sizeof(List));
  p->item = x;      // l - variable maintaining access to the head of the list
  p->next = *l;     // just have to update pointer
  *l = p;           // pointer to the head of the list
}

List *predecessor_list(List *l, listItem x) {
  if((l == NULL) || (l->next == NULL)) {
    printf("Error: Null list.\n");
    return (NULL);
  }
  if((l->next)->item == x)
    return(l);
  else
    return(predecessor_list(l->next, x));
}

void delete_list(List **l, listItem x) {
  List *p; // item pointer
  List *pred; // predecessor pointer
  List *search_list(), *predecessor_list();

  p = search_list(*l, x);
  if(p != NULL) {
    pred = predecessor_list(*l, x);
    if(pred == NULL)
      *l = p->next;
    else
      pred->next = p->next;
    free(p); // free memory used by deleted node
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
  List *start = malloc(sizeof(List));
  start->item = 5;

  insert_list(&start, 10);
  insert_list(&start, 20);
  insert_list(&start, 30);
  print_list(start);

  List *second = start->next;

  printf("First item: %d\n", start->item);
  printf("Second item: %d\n", second->item);
  printf("Predecessor of 10: %d\n", (predecessor_list(start, 10)->item));
  printf("Item next to searched node (20): %d\n", (search_list(start, 20)->next)->item);

  delete_list(&start, 20);
  printf("Deleted 20 from the list.\n");
  print_list(start);

  insert_list(&start, 15);
  printf("Inserted 15 into the list.\n");
  insert_list(&start, 10);
  insert_list(&start, 25);
  print_list(start);
  printf("Predecessor of 10: %d\n", (predecessor_list(start, 10)->item));

  free(start);
  return 0;
}
