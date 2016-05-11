def fib(n,a,b):
    if n == 0:
        return a
    else:
        return fib(n-1, a+b, a)

n = int(input("> "))
print(fib(n,0,1))
