maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- how many elements does it take for the sum of the roots of all natural
-- numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<= 1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function composition:
-- this
  --map (\x -> negate (abs x)) [1, 2, 3, 4, 5]
-- is equivalent to
  --map (negate . abs) [1, 2, 3, 4, 5]

-- find sum of all odd squares smaller than 10,000
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (< 10000) . filter odd . map (^2) $ [1..]
