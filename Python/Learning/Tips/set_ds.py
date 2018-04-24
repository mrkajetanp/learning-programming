some_list = ['a', 'b', 'c', 'b', 'd', 'm', 'n', 'n']
duplicates = set([x for x in some_list if some_list.count(x) > 1])
print("duplicates:", duplicates)

valid = set(['yellow', 'red', 'blue', 'green', 'black'])
input_set = set(['red', 'brown', 'blue'])
print("intersection:", input_set.intersection(valid))
print("difference:", input_set.difference(valid))

a_set = {'red', 'blue', 'green', 'blue'}
print("set:", a_set)
