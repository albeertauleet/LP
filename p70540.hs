data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = ((eval1 x) + (eval1 y))
eval1 (Sub x y) = ((eval1 x) - (eval1 y))
eval1 (Mul x y) =  ((eval1 x) * (eval1 y)) 
eval1 (Div x y) = (div (eval1 x) (eval1 y))


eval2 :: Expr -> Maybe Int
eval2 (Val x) = return x
eval2 (Add x y) = do
    t <- eval2 x
    e <- eval2 y
    return (t + e)
eval2 (Sub x y) = do
    t <- eval2 x
    e <- eval2 y
    return (t - e)
eval2 (Mul x y) = do    
    t <- eval2 x
    e <- eval2 y
    return (t * e)
eval2 (Div x y) = do
    t <- eval2 x
    e <- eval2 y 
    if e == 0 then do 
        Nothing
    else 
        return (div t e)

eval3 :: Expr -> Either String Int
eval3 (Val x) = return x
eval3 (Add x y) = do
    t <- eval3 x
    e <- eval3 y
    Right (t + e)
eval3 (Sub x y) = do
    t <- eval3 x
    e <- eval3 y
    Right (t - e)
eval3 (Mul x y) = do    
    t <- eval3 x
    e <- eval3 y
    Right (t * e)
eval3 (Div x y) = do
    t <- eval3 x
    e <- eval3 y 
    if e == 0 then do 
        Left "div0"
    else 
        Right (div t e)