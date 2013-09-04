from euler import number
from itertools import permutations, islice
print number(*(islice(permutations(range(10)), 999999, 1000000)))
