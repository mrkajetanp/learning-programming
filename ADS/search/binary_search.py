#!/usr/bin/env python3

def binary_search(arr, item):
    start = 0
    end = len(arr) - 1

    while start <= end:
        mid = (start + end) // 2
        curr = arr[mid]

        if curr == item:
            return mid
        elif item < curr:
            end = mid - 1
        else:
            start = mid + 1

    return None

print(binary_search([1, 3, 4, 8, 9, 12, 15, 18, 22, 26, 29], 12))
print(binary_search([1, 3, 4, 8, 9, 12, 15, 18, 22, 26, 29], 8))
print(binary_search([1, 3, 4, 8, 9, 12, 15, 18, 22, 26, 29], 22))
