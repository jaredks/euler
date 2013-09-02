from operator import itemgetter

def collatz(n):
    if n in cache:
        return cache[n]
    c = cache[n] = collatz(3*n + 1 if n % 2 else n / 2) + 1
    return c

cache = {1: 1}
print max(((a, collatz(a)) for a in xrange(1, 10**6)), key=itemgetter(1))[0]
