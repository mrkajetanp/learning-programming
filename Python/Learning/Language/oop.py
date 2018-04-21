
class C1(object):
    x = 23

    def method_one():
        print("C1: {}".format(C1.x*2))


class C2(object):
    def __init__(self, n):
        self.x = n

    def double_value(self):
        return 2*self.x


print(C1.x)
C1.method_one()
inst = C2(21)
print(inst.double_value())
