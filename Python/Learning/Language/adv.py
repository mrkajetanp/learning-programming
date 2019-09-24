
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

def rules(rules_filename):
    with open(rules_filename, encoding='utf-8') as pattern_file:
        for line in pattern_file:
            pattern, search, replace = line.split(None, 2)
            yield build_match_and_apply_functions(pattern, search, replace)

def plural(noun, rules_filename='plural5-rules.txt'):
    for matches_rule, apply_rule in rules(rules_filename):
        if matches_rule(noun):
            return apply_rule(noun)
    raise ValueError('no matching rule for {0}'.format(noun))

class Fib:
    '''iterator that yields numbers in the Fibonacci sequence'''

    def __init__(self, max):
        self.max = max

    def __iter__(self):
        self.a = 0
        self.b = 1
        return self

    def __next__(self):
        fib = self.a
        if fib > self.max:
            raise StopIteration
        self.a, self.b = self.b, self.a + self.b
        return fib

import itertools

print(list(itertools.product('ABC', '123')))
print(list(range(0, 3)))
print(list(zip(range(0, 3), range(10, 13))))

eval("__import__('math').sqrt(5)", {"__builtins__":None}, {})
