
# [Lecture] CS1P - Code style

# [Lecture] CS1P - Programming habits

# [Lecture] CS1F - ER diagrams

# [Lecture] CS1F - ER diagram exercises


for i in range(0, 10, 2):
    print(i)

for i in range(10):
    print(i)

    if i%3 == 0:
        continue

    print("*" * i)


# [Tutorial] CS1F - Database design

# [Tutorial] CS1F - ER diagram

# [Tutorial] CS1F - ER diagram - Entities & attributes

# [Tutorial] CS1F - ER diagram - Relationships

# [Lab] CS1P - Control flow statements

# [Lab] CS1P - Monte Carlo approximation

# [Lecture] CS1F - ER diagram - Cardinality

# [Lecture] CS1F - ER Diagram - Advanced

# [Lecture] CS1P - Reasoning about the control flow

# [Lecture] CS1P - Intricacies of loops

# [Lecture] CS1F - Database integrity

# [Lecture] CS1F - Database constraints

# [Lecture] CS1P - Elegance of code

# [Lecture] CS1P - Functions & program structure

def test(a):
    return a, a*2, a*3

one, two, three = test(10)
x = test(5)

print("{} {} {}".format(one, two, three))
print(x)

print(list(sorted(reversed(test(3)))))
