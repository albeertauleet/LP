data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)


size :: Tree a -> Int 
size Empty = 0
size (Node _ a b) = 1 + size a + size b

height :: Tree a -> Int
height Empty = 0
height (Node _ a b) = 1+  (max (height a) (height b))

equal :: Eq a => Tree a -> Tree a -> Bool 
equal  = (==)

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    (Node a n1 n2) ==  (Node b b1 b2) | a == b = n1 == b1 && n2 == b2
                                        | otherwise = False
    _ == _ = False

isomorphic :: Eq a => Tree a -> Tree a -> Bool 
isomorphic Empty Empty = True
isomorphic Empty (Node _ _ _) = False
isomorphic (Node _ _ _) Empty = False
isomorphic (Node a n1 n2) (Node b b1 b2) =  a == b && ((isomorphic n1 b1 && isomorphic n2 b2)|| (isomorphic n1 b2 && isomorphic n2 b1))


preOrder :: Tree a -> [a] 
preOrder Empty = []
preOrder (Node a n1 n2) = [a] ++ (preOrder n1) ++ (preOrder n2)

postOrder :: Tree a -> [a]
postOrder Empty = [] 
postOrder (Node a n1 n2) = postOrder n1 ++ postOrder n2 ++ [a]

inOrder :: Tree a -> [a] 
inOrder Empty = []
inOrder (Node a n1 n2) = inOrder n1 ++ [a] ++ inOrder n2

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst a = bf [a]
    where 
        bf [] = []
        bf (Empty : xs) = bf xs
        bf ((Node a x1 x2):xs) = a: bf(xs ++ [x1, x2])


build :: Eq a => [a] -> [a] -> Tree a 
build [] [] = Empty
build (x:xs) ys = Node x (build leftPreorder leftInorder) (build rightPreorder rightInorder)
    where
        (leftInorder, _:rightInorder) = span (/= x) ys
        leftPreorder = take (length leftInorder) xs
        rightPreorder = drop (length leftInorder) xs


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ Empty a = a
overlap _ a Empty = a
overlap fun (Node a a1 a2) (Node b b1 b2) = Node (fun a b) (overlap fun a1 b1) (overlap fun  a2 b2)