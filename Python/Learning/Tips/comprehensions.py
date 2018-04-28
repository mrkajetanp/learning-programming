
# Dict comprehension
some_dict = {'a': 5, 'b': 8, 'c': 9}
print({v: k for k, v in some_dict.items()})

# Set comprehension
print({x**2 for x in [1, 1, 2, 3]})
