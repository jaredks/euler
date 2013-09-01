from euler import fibs
print sum(n for n in fibs(4000000) if not n & 1)
