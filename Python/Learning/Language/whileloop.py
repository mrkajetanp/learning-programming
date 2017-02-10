i = 0
numbers = []

while i < 6:
    print("At the top i is", i)
    numbers.append(i)

    i += 1
    print("Numbers now:", numbers)
    print("At the bottom i is", i)

print("The numbers:")
for num in numbers:
    print(num)

print('Finished!')

