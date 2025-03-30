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

