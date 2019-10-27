
# [Lab] CS1P - Basic list operations

# [Lab] CS1P - Advanced lists and functions

# [Lecture] CS1F - Relational algebra in SQL

# [Lecture] CS1F - Aspects of relational algebra

# [Lecture] CS1P - Aspects of Lists

# [Lecture] CS1P - Tuples

x = [1, 2, 4]
print(x.pop(0))

'a' in "abcdefghijklmnopqrsyxz"

# [Lecture] CS1F - Databases - Others

# [Lecture] CS1F - Databases - Miscellaneous

synonyms = { "chemistry": "science", "physics": "science", "tea": "drinks", "coffee": "drinks"}
keywords = { "science": "upstairs", "drinks": "downstairs", "test": "nope" }

print("science" in keywords)

s = "drinks"
print(keywords[synonyms[s]] if s in synonyms else keywords[s])

print()

for x in keywords:
    print("{} -> {}".format(x, keywords[x]))

print(len(keywords))

for k, v in keywords.items():
    print("{} -> {}".format(k, v))

tup = (0, 1, 2, 3, 4, 5)
print(tup[-1])
print(tup[1:-1])
