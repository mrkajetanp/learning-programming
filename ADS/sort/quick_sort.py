#!/usr/bin/env python3

def quick_sort(arr):
    if len(arr) < 2:
        return arr

    # last element as first pivot
    pivot = arr.pop()
    greater = [] # greater than pivot
    lesser = [] # less / equal to pivot

    for x in arr:
        (greater if x > pivot else lesser).append(x)

    return quick_sort(lesser) + [pivot] + quick_sort(greater)


print(quick_sort([6, 3, 5, 1, 4, 7, 10, 10, 1, 8, 13, 11]))
print(quick_sort([4, 3, 2, 1, 5, 6, 8, 9, 10]))
