#include <stdio.h>

void swap (int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

void insertionSort (int arr[], int n) {
    int i, j;
    for (i = 1 ; i < n ; ++i) {
        j = i;
        while ((j > 0) && (arr[j-1] > arr[j])) {
            swap (&arr[j], &arr[j-1]);
            j--;
        }
    }
}

void printArray (int arr[], int n) {
    int i;
    for (i = 0 ; i < n ; ++i)
        printf ("%d ", arr[i]);
    printf ("\n");
}

int main () {
    int arr[] = {4, 1, 8, 10, 5, 2, 3, 6};
    printArray (arr, 8);
    insertionSort (arr, 8);
    printArray (arr, 8);

    return 0;
}
