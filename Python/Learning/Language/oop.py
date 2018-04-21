
class C1(object):
    x = 23

    def method_one():
        print("C1: {}".format(C1.x*2))


print(C1.x)
C1.method_one()
