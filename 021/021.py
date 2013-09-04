from euler import divisors

def sum_proper_divisors(n):
    return sum(divisors(n)) - n

def amicable_number(a):
    b = sum_proper_divisors(a)
    return a != b and sum_proper_divisors(b) == a

if __name__ == "__main__":
    print sum(a for a in xrange(2, 10000) if amicable_number(a))
