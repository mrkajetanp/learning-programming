#!/usr/bin/env python3

def merge_sort(arr):

    def merge(left, right):

        def _merge():
            while left and right:
                yield (left if left[0] <= right[0] else right).pop(0)
            yield from left
            yield from right

        return list(_merge())

    if len(arr) <= 1:
        return arr

    mid = len(arr) // 2

    return merge(merge_sort(arr[:mid]), merge_sort(arr[mid:]))


def merge_sort_2(arr):

    def merge(left, right):
        res = []

        while left and right:
            res.append((left if left[0] <= right[0] else right).pop(0))

        res.extend(left)
        res.extend(right)

        return res

    if len(arr) <= 1:
        return arr

    mid = len(arr) // 2

    return merge(merge_sort_2(arr[:mid]), merge_sort_2(arr[mid:]))


# print(merge_sort([6, 3, 5, 1, 4, 7, 10, 10, 1, 8, 13, 11]))
print(merge_sort_2([6, 3, 5, 1, 4, 7, 10, 10, 1, 8, 13, 11]))
