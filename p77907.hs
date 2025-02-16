sqrtInt :: Int -> Int
sqrtInt n = floor (sqrt (fromIntegral n))


absValue :: Int -> Int
absValue n
    |  n >= 0 = n
    | otherwise = -n

power:: Int -> Int -> Int
power _ 0 = 1
power base exponent = base*power base (exponent - 1)


isPrime:: Int -> Bool
isPrime n | n <= 1 = False
          | n == 2 = True
          | n == 3 = True
          | mod n 2 == 0 = False
          | otherwise = isPrimeAux n 3

isPrimeAux:: Int -> Int -> Bool
isPrimeAux n i | i*i > n = True
               | mod n i == 0 = False
               | otherwise = isPrimeAux n (i+2)

slowFib:: Int -> Int
slowFib n   | n == 0 = 0
            | n == 1 = 1
            | otherwise = slowFib(n-1) + slowFib(n-2)

quickFib:: Int -> Int
quickFib n = fst (quickFibAux n)


quickFibAux :: Int -> (Int, Int)
quickFibAux 0 = (0, 1)
quickFibAux n = (b, a + b)
  where
    (a, b) = quickFibAux (n - 1)