
def test_var_args(f_arg, *argv):
    print("normal:", f_arg)
    for arg in argv:
        print("another one:", arg)


def greet_me(**kwargs):
    for key, value in kwargs.items():
        print("{0} = {1}".format(key, value))


test_var_args('hello', 'there', 'general', 'Kenobi')
greet_me(name="test", surname="me")
