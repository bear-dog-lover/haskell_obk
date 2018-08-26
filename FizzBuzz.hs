import qualified Data.Map as Map

fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0 = "fizzbuzz"
    | n `mod`  3 == 0 = "fizz"
    | n `mod`  5 == 0 = "buzz"
    | otherwise       = show n

main = do
    putStr $ concatMap (++"\n") (map fizzbuzz [1..30])