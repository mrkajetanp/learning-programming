#include <stdio.h>

int binarySearch (int arr[], int key, int low, int high) {
    int mid;
    if (low > high) return (-1); /* not found */
    mid = (low + high) / 2;
    if (arr[mid] == key)   return (mid);
    if (arr[mid] > key)
        return (binarySearch(arr, key, low, mid-1));
    else
        return (binarySearch(arr, key, mid+1, high));
}

int main () {
    int arr[] = {1, 4, 5, 6, 8, 10, 12};
    printf ("%d\n", binarySearch(arr, 6, 0, 6));
    return 0;
}
