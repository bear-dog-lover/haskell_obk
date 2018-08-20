multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlpanum :: Char -> Bool
isUpperAlpanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in qsort smallerOrEqual ++ [x] ++ qsort larger

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd  n = n : chain (n * 3 + 1)

-- sum [1 | m <- filter (>=15) [length (chain n) | n <- [1..100]]]

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChains2 :: Int
numLongChains2 = length (filter (\xs -> length xs > 15)
                                (map chain [1..100]))

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs --2引数関数　アキュムレータ　畳み込み対象のリスト

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

concat' :: (Num a) => [a] -> a
concat' = foldl (\acc b -> acc * 10 + b) 0

map'' :: (a -> b) -> [a] -> [b]
--map'' f xs = foldr (\x acc -> f x : acc) [] xs
map'' f = foldr (\x acc -> f x : acc) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldr (\y acc -> if x == y then True else acc) False xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldl1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
