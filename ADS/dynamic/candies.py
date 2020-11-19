#!/usr/bin/env python3

def candies(n, arr):
    candy = [1 for i in range(len(arr))]

    for i in range(1, len(arr)):
        if arr[i-1] < arr[i]:
            candy[i] = candy[i-1]+1

    print(arr)
    print(candy)

    for i in range(len(arr)-2, -1, -1):
        if arr[i] > arr[i+1]:
            candy[i] = candy[i+1]+1

    print(candy)

    return sum(candy)

print(candies(3, [1, 2, 2]))
print(candies(10, [2, 4, 2, 6, 1, 7, 8, 9, 2, 1]))
print(candies(8, [2, 4, 3, 5, 2, 6, 4, 5]))
