puts (1..999).inject{|sum, n| (n % 3 == 0 || n % 5 == 0) and sum + n or sum}
