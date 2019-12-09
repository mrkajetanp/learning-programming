
# [Lab] CS1P - Lab exam in practice

# [Lab] CS1P - Lab exam adjustments

# [Lecture] CS1F - Revision lecture - HCI 1

# [Lecture] CS1F - Revision lecture - HCI 2

# [Lecture] CS1P - Exam material - Introduction

# [Lecture] CS1P - Exam material - Advanced

# [Lecture] CS1F - Revision - SQL

# [Lecture] CS1F - Revision - Relational Algebra

# [Lecture] CS1P - Revision - Higher order functions

# [Lecture] CS1P - Revision - Algorithms

# [Revision] CS1F - Relational Algebra Quiz

# [Revision] CS1F - SQL Quiz

# [Revision] CS1F - IM Theoretical Material

# [Revision] CS1F - IM Practical Material


print([1, 2, 3][0:2])

def certify(s, certification="CERTIFIED"):
    return s + " " + certification

def combine(s):
    return "-".join(s)

def package(elts, pre="(", post=")"):
    return combine([pre+c+post for c in elts])

values = ["one", "two", "three"]
print(certify(package(values, "[", "]")))

def flatten(l):
    stack = list(l)
    result = []
    while len(stack) > 0:
        first = stack.pop(0)
        print("Both", first, stack)
        if type(first) == type([]):
            stack = first + stack
            print("Stack", stack)
        else:
            result.append(first)
            print("Result", result)
    return result

print(flatten([[["a", "b"], "c", [["d"]]]]))
