#include <stdio.h>
#include <time.h>

clock_t start, end;
double cpu_time_used;

void swap(int *a, int *b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}

int partition(int s[], int l, int h) {
  int i;         // counter
  int p;         // pivot element index
  int firsthigh; // divided position for pivot element

  p = h;
  firsthigh = l;
  for(i = l ; i < h ; i++)
    if(s[i] < s[p]) {
      swap(&s[i], &s[firsthigh]);
      firsthigh++;
    }
  swap(&s[p], &s[firsthigh]);
  return (firsthigh);
}

void quicksort(int s[], int l, int h) {
  int p;      // index of partition
  if((h-l) > 0) {
    p = partition(s,l,h);
    quicksort(s,l,p-1);
    quicksort(s,p+1,h);
  }
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

int main() {
  start = clock();
  int arr[10] = {3,2,4,65,9,11,67,8,15,33};
  int arr_size = sizeof(arr)/sizeof(int);

  for (int i = 0 ; i < arr_size ; i++)
    printf("%d ", arr[i]);
  printf("\n");

  quicksort(arr, 0, arr_size);

  for (int i = 0 ; i < arr_size ; i++)
    printf("%d ", arr[i]);
  printf("\n");

  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("%f\n", cpu_time_used);

  return 0;
}
