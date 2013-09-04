from euler import fibs, number_digits
print next(i for i, n in enumerate(fibs()) if number_digits(n) == 1000)
