#include <stdio.h>
#include <stdlib.h>

void merge (int a[], int low, int mid, int high) {
  int b[10000];
  int i = low, j = mid + 1, k = 0;

  while (i <= mid && j <= high) {
    if (a[i] <= a[j])
      b[k++]  = a[i++];
    else
      b[k++] = a[j++];
  }
  while (i <= mid)
    b[k++] = a[i++];

  while (j <= high)
    b[k++] = a[j++];

  k--;
  while (k >= 0) {
    a[low + k] = b[k];
    k--;
  }
}

void mergeSort (int a[], int low, int high) {
  if (low < high) {
    int m = (high+low)/2;
    mergeSort(a,low,m);
    mergeSort(a,m+1,high);
    merge(a,low,m,high);
  }
}

void printArray(int A[], int size) {
    int i;
    for (i=0; i < size; i++)
        printf("%d ", A[i]);
    printf("\n");
}

int main() {
  int arr[] = {12, 11, 13, 5, 6, 7, 9};
    int arr_size = sizeof(arr)/sizeof(arr[0]);

    printf("Given array is \n");
    printArray(arr, arr_size);

    mergeSort(arr, 0, arr_size - 1);

    printf("\nSorted array is \n");
    printArray(arr, arr_size);
    return 0;
}
