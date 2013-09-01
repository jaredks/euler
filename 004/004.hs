isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

main :: IO ()
main = print $ maximum [x*y | x <- [900..999], y <- [x..999], isPalindrome (x*y)]
