fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib(n - 1) + fib(n - 2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = [x] ++ repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = [(a, b)] ++ zip' as bs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = a == x || a `elem'` xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger         = [a | a <- xs, a > x]
    in qsort smallerOrEqual ++ [x] ++ qsort larger