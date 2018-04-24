from functools import reduce

items = [1, 2, 3, 4, 5]
squared = list(map(lambda x: x**2, items))
print("squared:", squared)

items = [1, -2, 3, 4, -5]
negative = list(filter(lambda x: x < 0, items))
print("negative:", negative)

items = [1, 2, 3, 4]
product = reduce(lambda x, y: x*y, items)
print("product:", product)
