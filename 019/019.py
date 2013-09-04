import datetime
print sum(datetime.datetime(y, m, 1).weekday() == 6 for y in xrange(1901, 2001) for m in xrange(1, 13))
