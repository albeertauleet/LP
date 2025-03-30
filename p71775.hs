countIf :: (Int -> Bool) -> [Int] -> Int
countIf _ [] = 0
countIf cond (l:ls)
    | (cond l) = 1 + countIf cond ls
    | otherwise = countIf cond ls

pam :: [Int] -> [Int -> Int] -> [[Int]]     
pam xs fs = [ map f xs | f <- fs ]


pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = [ map (flip ($) x) fs | x <- xs ]

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl  pr op ini l = foldl op ini (filter pr l)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert _ [] e = [e]
insert cond (l:ls) e 
    | cond l e = l : (insert cond ls e)
    | otherwise = e:l:ls

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort _ [] = []
insertionSort cond (l:ls) = insert cond (insertionSort cond ls) l