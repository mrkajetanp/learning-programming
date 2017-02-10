#include <stdio.h>
#include <stdlib.h> // qsort
#include <string.h> // strcmp

/* insertion sort functions */
void swap(int* a, int* b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}

void insertion_sort(int arr[], int n) {
  int i, j; /* counters */
  
  for(i = 1 ; i < n ; i++) {
    j=i;
    while ((j>0) && (arr[j] < arr[j-1])) {
      swap(&arr[j], &arr[j-1]);
      j--;
    }
  }
}

/* string sort functions */
static int myCompare (const void * a, const void * b) {
  return strcmp (*(const char **) a, *(const char **) b);
}
void string_sort(const char *arr[], int n) {
  qsort (arr, n, sizeof (const char *), myCompare);
}

int main() {

  /* sorting array of ints with insertion sort */
  int arrint[10] = {12, 76, 3, 1, 33, 88, 92, 2, 7, 4};
  insertion_sort(arrint, 10);
  for(int i = 0 ; i < 10 ; i++)
    printf("%d ", arrint[i]);
  puts("");
  /* sorting array of strings with qsort */
  const char *strarr[] = {"bbb", "ccc", "aaa"};
  int n = sizeof(strarr)/sizeof(strarr[0]);
  string_sort(strarr, n);
  for (int i = 0; i < n; i++)
    printf("%s ", strarr[i]);
  puts("");
  return 0;
}