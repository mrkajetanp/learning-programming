
class C1(object):
    x = 23

    def method_one():
        print("C1: {}".format(C1.x*2))


class C2(object):
    def __init__(self, n):
        self.x = n

    def double_value(self):
        return 2*self.x


class C3(object):
    def hello_there(self):
        print("General Kenobi!")


class Singleton(object):
    _singletons = {}

    def __new__(cls, *args, **kwds):
        if cls not in cls._singletons:
            cls._singletons[cls] = super(Singleton, cls).__new__(cls)
        return cls._singletons[cls]

    def __init__(self, n):
        self.x = n

    def double_value(self):
        return 3*self.x


class Node(object):
    def __init__(self, id):
        self.id = id

    def __str__(self):
        return str("Node id: {}".format(self.id))


print(C1.x)
C1.method_one()
inst = C2(21)
print(inst.double_value())
C3().hello_there()

x = Singleton(8)
print(x.double_value())
y = Singleton(9)
print(x.double_value(), y.double_value())

n = Node(8)
print(n)
