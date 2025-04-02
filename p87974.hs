main :: IO()
main = do
    nom <- getChar
    if nom == 'A' || nom == 'a' then putStrLn "Hello!" else putStrLn "Bye!"