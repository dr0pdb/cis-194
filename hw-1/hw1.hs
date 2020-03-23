{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = [n `mod` 10] ++ toDigitsRev (n `div` 10)


-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther xs = doubleEveryOther (init (init xs)) ++ [2 * (last (init xs)), last xs]


-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = (x `mod` 10) + (x `div` 10) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- Main driver
main :: IO ()
main =  do

print(toDigits 1234 == [1,2,3,4])
print(toDigitsRev 1234 == [4,3,2,1])
print(toDigits 0 == [])
print(toDigits (-17) == [])

print(doubleEveryOther [8,7,6,5] == [16,7,12,5])
print(doubleEveryOther [1,2,3] == [1,4,3])

print(sumDigits [16,7,12,5] == 22)
print(sumDigits [1, 14, 2, 17, 9] == 25)

print(validate 4012888888881881 == True)
print(validate 4012888888881882 == False)

print(hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")])
