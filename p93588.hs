myMap :: (a->b)->[a]->[b]
myMap o l = [o x | x <- l] 

myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter cond l = [x | x <- l, cond x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op l1 l2 = [x `op` y | (x, y) <- zip l1 l2 ]

thingify :: [Int] -> [Int] -> [(Int, Int)] 
thingify l1 l2 = [(x,y) | x <- l1, y <- l2, x `mod` y == 0]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]