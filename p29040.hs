insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert (x:xs) n 
    | x >= n = n : (x : xs)
    | otherwise = x : insert xs n

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:xs) n 
    | x == n = xs
    | otherwise = x : remove xs n

ssort :: [Int] -> [Int]
ssort [] = []
ssort x = m : (ssort (remove x m))
    where m = minimum x

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)  
    | otherwise = y : merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = 
    let (left, right) = splitAt (length xs `div` 2) xs  
    in merge (msort left) (msort right)

qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort left ++ [mediana] ++ qsort right
    where
        left = [y | y <- xs, y <= x]
        right = [y | y <- xs, y > x]
        mediana = x


genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort [x] = [x]
genQsort (x:xs) = genQsort left ++ [mediana] ++ genQsort right
    where
        left = [y | y <- xs, y <= x]
        right = [y | y <- xs, y > x]
        mediana = x