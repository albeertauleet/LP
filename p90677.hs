myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ ini [] = ini  
myFoldl op ini (l:ls) = myFoldl op (ini `op` l) ls

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ ini [] = ini
myFoldr op ini (l:ls) = l `op` myFoldr op ini ls

myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a 
myUntil c f n 
    | c n  = n 
    | otherwise = myUntil c f (f n) 

myMap :: (a -> b) -> [a] -> [b] 
myMap _ [] = []
myMap f (l:ls) = (f l) : myMap f ls

myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter _ [] = []
myFilter condicio (l:ls)
    | (condicio l) = l : myFilter condicio ls
    | otherwise = myFilter condicio ls

myAll :: (a -> Bool) -> [a] -> Bool 
myAll _ [] = True
myAll condicio (l:ls)
    | (condicio l) = myAll condicio ls
    | otherwise = False

myAny :: (a -> Bool) -> [a] -> Bool 
myAny _ [] = False
myAny condicio (l:ls)
    | (condicio l) = True
    | otherwise = myAny condicio ls

myZip :: [a] -> [b] -> [(a, b)] 
myZip [] _ = []
myZip _ [] = []
myZip (l:ls) (x:xs) = (l, x) : myZip ls xs 

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith op (l:ls) (x:xs) = (op l x) : (myZipWith op ls xs)