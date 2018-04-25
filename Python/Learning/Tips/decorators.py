from functools import wraps


# Basic


def a_decorator(func):
    def wrap_fn():
        print("Before function")
        func()
        print("After function")

    return wrap_fn


@a_decorator
def a_function_to_decorate():
    """Decorator example!"""
    print("Decorating that guy here!")


a_function_to_decorate()
print(a_function_to_decorate.__name__)
print()

# Lib


def a_decorator(func):
    @wraps(func)
    def wrap_fn():
        print("Before function")
        func()
        print("After function")

    return wrap_fn


@a_decorator
def a_function_to_decorate():
    """Decorator example!"""
    print("Decorating that guy here!")


a_function_to_decorate()
print(a_function_to_decorate.__name__)
print()
