print next(int(a*b*(a**2+b**2)**0.5) for a in xrange(1, 380) for b in xrange(a) if a+b+(a**2+b**2)**0.5 == 1000)
