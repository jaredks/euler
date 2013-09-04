from euler import divisors
from itertools import combinations_with_replacement

def sum_proper_divisors(n):
    return sum(divisors(n)) - n

def abundant(n):
    return sum_proper_divisors(n) > n

if __name__ == "__main__":
    limit = 28123
    abundant_numbers = filter(abundant, xrange(1, limit+1))
    number_is_sum_of_two_abundants = {sum(combo) for combo in combinations_with_replacement(abundant_numbers, 2)}
    print sum(n for n in xrange(1, limit+1) if n not in number_is_sum_of_two_abundants)
