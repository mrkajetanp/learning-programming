from pprint import pprint
import itertools

my_dict = {'name': 'Yasoob', 'age': 'undefined', 'personality': 'great'}
pprint(my_dict)

a_list = [[1, 2], [3, 4], [5, 6]]
print(list(itertools.chain.from_iterable(a_list)))


class A(object):
    def __init__(self, a, b, c, d, e, f):
        self.__dict__.update({k: v for k, v in locals().items() if k != 'self'})


a = A(1, 2, 3, 4, 5, 6)
print(a.a, a.b, a.c, a.d, a.e, a.f)
