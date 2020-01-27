
# [Lab] CS1P - Semester 1 Review

# [Lab] CS1P - Semester 1 Exercises

# [Lecture] CS1S - Basic hardware circuits

# [Lecture] CS1S - Logic gates

# [Lecture] CS1P - Matrices

# [Lecture] CS1P - List operations

def occurrences(l):
    result = {}

    for x in l:
        if x in result:
            result[x] = result[x]+1
        else:
            result[x] = 1

    return result

print(occurrences([1, 2, 1, 1, 3, 4, 3, 2]))

# [Lecture] CS1S - Boolean algebra

# [Lecture] CS1S - Boolean operations

# [Lecture] CS1P - Feedback

# [Lecture] CS1P - Time complexity

def bubble_sort(l):
    result = l
    for i in range(len(result)):
        for j in range(i, len(result)):
            if result[i] < result[j]:
                result[i], result[j] = result[j], result[i]
    return l


print(bubble_sort([1, 2, 7, 5, 8, 4, 3, 2]))

