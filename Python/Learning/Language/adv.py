
import math

math.sin(math.pi / 2)

a_list = ['a', 'b', 'c']
a_list.extend(['d', 'e', 'f'])

a_list = ['aa', 'bb', 'cc', 'dd', 'aa']

print("Count: {}".format(a_list.count('aa')))

a_set = {1, 2}
a_set.add(4)

print(a_set)

a_set.update({2, 4, 6, 10, 12, 20})
print(a_set)

a_set.discard(10)

b_set = a_set.copy()

b_set.add(7)
b_set.discard(6)

print(a_set)
print(b_set)

print(b_set.intersection(a_set))
