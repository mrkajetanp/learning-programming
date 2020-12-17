#!/usr/bin/env python3

def counting_sort(arr, maxval):
    counts = [0 for x in range(maxval+1)]

    for x in arr:
        counts[x] += 1

    result = [-1] * len(arr)

    for i in range(1, len(counts)):
        counts[i] = counts[i-1] + counts[i]

    for x in arr:
        result[counts[x]-1] = x
        counts[x] -= 1

    return result

print(counting_sort([4, 3, 2, 1, 5, 6, 8, 2], 10))
print(counting_sort([6, 3, 5, 1, 4, 0, 7, 10, 5, 10, 1, 8, 13, 11, 1], 15))
print(counting_sort([4, 3, 2, 1, 5, 6, 8, 9, 10, 6, 5, 3, 6, 1], 15))
