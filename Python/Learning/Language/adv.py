
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

import os

print(os.path.expanduser('~'))

pathname = '/Users/pilgrim/diveintopython3/examples/humansize.py'

(dirname, filename) = os.path.split(pathname)
print(os.path.splitext(filename))

print(os.getcwd())

pattern = "^M?M?"

import re

print(pattern)
print(re.search(pattern, 'M'))

phonePattern = re.compile(r'^(\d{3})-(\d{3})-(\d{4})-(\d+)$')
print(phonePattern.search('800-555-1212-1234').groups())
