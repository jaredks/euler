primes = 2 : sieve primes [3..]
    where
        sieve (p:ps) xs = let (h,t) = span (< p*p) xs
                          in h ++ sieve ps [x | x<-t, rem x p /= 0]

main :: IO ()
main = print $ last (take 10001 primes)
