from euler import number_divisors

def triangle_numbers():
    a, b = 0, 0
    while 1:
        a, b = a+b, b+1
        yield a

print next(n for n in triangle_numbers() if number_divisors(n) > 500)
