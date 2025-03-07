flatten :: [[Int]] -> [Int]
flatten [] = []
flatten ((l):ls) = l ++ (flatten ls)

myLength :: String -> Int
myLength [] = 0
myLength (s:ss) = 1 + (myLength ss)

myReverse :: [Int] -> [Int]
myReverse l = reverse l

countIn :: [[Int]] -> Int -> [Int]
countIn  [] _ = []
countIn (l:ls) n = countInAux l n : countIn ls n

countInAux :: [Int] -> Int -> Int
countInAux [] _ = 0
countInAux (l:ls) n 
    | l == n = countInAux ls n + 1
    | otherwise = countInAux ls n

firstWord :: String -> String
firstWord x = takeWhile (/= ' ') (dropWhile (== ' ') x)