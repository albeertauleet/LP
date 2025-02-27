myLength :: [Int] -> Int
myLength  [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum (x:xs) =  myMaximumAux (xs) x

myMaximumAux :: [Int] -> Int -> Int
myMaximumAux [] max = max
myMaximumAux (x:xs) max = if x > max 
                          then myMaximumAux xs x 
                          else myMaximumAux xs max


average :: [Int] -> Float
average (x:xs) = averageAux (xs) (fromIntegral x) 1

averageAux :: [Int] -> Float -> Float -> Float
averageAux [] sum n = sum/n
averageAux (x:xs) sum n = averageAux (xs) (sum+ fromIntegral x) (n+1)

buildPalindrome :: [Int] -> [Int]
buildPalindrome l = buildPalindromeAux l ++ l

buildPalindromeAux :: [Int] -> [Int]
buildPalindromeAux [] = []
buildPalindromeAux (x:xs) = buildPalindromeAux xs ++ [x]

remove ::  [Int] -> [Int] -> [Int]
remove [] [] = []
remove x [] = x
remove [] x = []
remove (x:xs) y | trobat x y = remove xs y
                | otherwise = x:(remove xs y)

trobat :: Int -> [Int] -> Bool
trobat x [] = False
trobat x (y:ys) | x == y = True
                | otherwise = trobat x ys

flatten :: [[Int]] -> [Int]
flatten l  =  flattenAux l []

flattenAux :: [[Int]] -> [Int] -> [Int]
flattenAux [] res = res
flattenAux (x:xs) res = flattenAux xs (res ++ x)

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens [] = ([], [])
oddsNevens (x:xs)    | (mod x 2) == 0 = (fst (oddsNevens xs), x:(snd (oddsNevens xs)))
                    | otherwise = ((x:(fst (oddsNevens xs))), snd (oddsNevens xs))

primeDivisors :: Int -> [Int]
primeDivisors n = filter (\x -> n `mod` x == 0) (takeWhile (<= n) primes)

isPrime :: Int -> Bool
isPrime n | n < 2 = False
          | otherwise = null [ x | x <- [2..(n-1)], n `mod` x == 0]


primes :: [Int]
primes = filter isPrime [2..]