#include <iostream>
#include <algorithm>

using std::cout;
using std::endl;
using std::swap;

void insertionSort(int arr[], int size) {
  int j;
  for (int i = 0 ; i < size ; i++) {
    j = i;
    while (j > 0 && arr[j-1] > arr[j]) {
      swap(arr[j], arr[j-1]);
      j--;
    }
  }
}

void insertionSortTwo(int arr[], int size) {
  int j, key;
  for (int i = 1 ; i < size ; i++) {
    key = arr[i];
    for (j = i-1 ; (j >= 0) && (arr[j] > key); j--) {
      arr[j+1] = arr[j];
    }
    arr[j+1] = key;
  }
  return;
}

int main() {
  int arr[] = {3,8,2,0,9,6,1,18,12};
  for (int i = 0 ; i < 9 ; i++)
    cout << arr[i] << " ";
  cout << endl;
  // insertionSort(arr, 9);
  insertionSortTwo(arr, 9);
  for (int i = 0 ; i < 9 ; i++)
    cout << arr[i] << " ";
  cout << endl;
  return 0;
}
