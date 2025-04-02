data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
create = Queue [] []


push :: a -> Queue a -> Queue a
push e (Queue q1 q2) = Queue q1 (e:q2)


pop :: Queue a -> Queue a
pop (Queue [] q2) = Queue (tail (reverse q2)) []
pop (Queue (q:q1) q2) = Queue q1 q2



top :: Queue a -> a
top (Queue [] q2) = last q2
top (Queue q1 _) = head q1



empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False


instance Eq a => Eq (Queue a)
    where
        Queue [][] == Queue [][] = True
        q1 == q2 
            | (empty q1 || empty q2) = False
            | otherwise = if (top q1) == (top q2) then ((pop q1) == (pop q2)) else False

instance Functor (Queue)
    where
        fmap f (Queue q1 q2) = (Queue (map f q1) (map f q2))

translation :: Num b => b -> Queue b -> Queue b 
translation f p =  fmap (+f) p


mergeQueue :: Queue a -> Queue a -> Queue a
mergeQueue (Queue q1 q2) (Queue q3 q4) = Queue (q1 ++ reverse q3) (q2 ++ reverse q4)
       



instance Applicative Queue 
    where
        (Queue [x] []) <*> q = (Queue [] [x]) <*> q
        (Queue [] [x]) <*> q = (Queue [x] []) <*> q
        pure x = (Queue [x] [])

instance Monad Queue
    where 
        return x  = Queue [x] []
        (Queue [] []) >>= f = (Queue [] [])
        (Queue q1 q2) >>= f = foldl mergeQueue (Queue [] []) (map f (q1 ++ reverse q2))


kfilter :: (p -> Bool) -> Queue p -> Queue p 
kfilter f q = do
    x <- q 
    if f x then return x else Queue [] []