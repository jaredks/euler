from euler import is_palindrome
print max(i*j for i in xrange(999, 900, -1) for j in xrange(999, i, -1) if is_palindrome(i*j))
