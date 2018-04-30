from functools import lru_cache

@lru_cache(maxsize=32)
def fib(n):
    if n < 2:
        return n
    return fib(n-1) + fib(n-2)

print([(fib(n), id(fib(n))) for n in range(10)])
print(fib.cache_info())
fib.cache_clear()
print([(fib(n), id(fib(n))) for n in range(10)])
print(fib.cache_info())
