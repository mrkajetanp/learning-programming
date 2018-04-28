add = lambda x, y: x + y
print(add(3, 5))
print((lambda x, y: x + y)(3, 9))

a = [(1, 2), (4, 1), (9, 10), (13, -3)]
a.sort(key=lambda x: x[1])
print(a)

one = [2, 4, 1, 3]
two = [5, 7, 8, 3]
data = list(zip(one, two))
data.sort()
print(data)
one, two = map(lambda t: list(t), zip(*data))
print(one, two)
