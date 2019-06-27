removeNonUppercase :: String -> String
removeNonUppercase s = [ c | c <- s, elem c ['A'..'Z'] ]


fizzBuzz :: Int -> [String]
fizzBuzz n 
  | n < 1     = []
  | otherwise = [ if (mod x 3 == 0 && mod x 5 == 0) then "FizzBuzz"
                  else if mod x 3 == 0 then "Fizz"
                    else if mod x 5 == 0 then "Buzz"
                      else show x | x <- [1..n] ]


rightTriangles :: [(Int, Int, Int)]
rightTriangles = [ (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b],
                               a^2 + b^2 == c^2, a+b+c == 24 ]


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= fat    = "Overweight"
  | otherwise     = "Obese"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)


myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a < b     = LT
  | b < a     = GT
  | otherwise = EQ


initials :: String -> String -> String
initials (f:_) (l:_) = [f]  ++ ". " ++ [l] ++ "."


calcBMIs :: (RealFloat a) => [(a, a)] -> [a]
calcBMIs xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea


-- finds the maximum element in a list
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "can't call maximum on an empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)


-- returns a list of elt replicated n times
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n elt
  | n <= 0    = []
  | otherwise = elt : replicate' (n - 1) elt


-- takes the first n elements from list xs
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ []    = []
take' n (x:xs)
  | n <= 0    = []
  | otherwise = x : take' (n - 1) xs


-- reverse a list
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]


-- creates an infinite list repeating element x
repeat' :: a -> [a]
repeat' a = a : repeat' a


-- zips two lists together into a list of tuples, truncating the longer one
-- e.g. zip' [1, 2, 3] ['a', 'b'] returns [(1, 'a'), (2, 'b')]
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs


-- checks whether e is an element of a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = elem' a xs


-- sorts a list, quickly
quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = 
  let left  = quicksort [a | a <- xs, a <= x]
      right = quicksort [a | a <- xs, a >  x]
  in left ++ [x] ++ right


-- zips two lists together using a custom function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs


-- returns a similar function with the first two arguments flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (a:as) = f a : map' f as


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let left  = quicksort (filter (<= x) xs)
      right = quicksort (filter (>  x) xs)
  in left ++ [x] ++ right


-- find the largest number under 100,000 that's divisible by 3829
specificNumber :: (Integral a) => a
specificNumber = head (filter p [100000,99999..])
  where p x = mod x 3829 == 0


-- find the sum of all odd squares smaller than 10,000
sumOddSquares :: (Integral a) => a
sumOddSquares = sum (takeWhile (< 10000) (filter odd (map (^2) [1,2..])))


-- for all starting numbers between 1 and 100, how many Collatz sequences have a
-- length greater than 15?
-- produce a Collatz chain given a number
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
  | even x = x : collatz (div x 2)
  | odd x  = x : collatz (x * 3 + 1)

-- find number of chains longer than 15 starting between 1 and x
longChains :: (Integral a) => a -> Int
longChains x
  | x < 1 = 0
  | otherwise = length (filter (\xs -> length xs > 15) (map collatz [1..x]))


sum'' :: (Num a) => [a] -> a
sum'' xs = foldl(\acc x -> acc + x) 0 xs

-- because functions are curried, this is equivalent to the above
sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0


elem'' :: (Eq a) => a -> [a] -> Bool
elem'' el = foldl (\acc x -> x == el || acc) False


map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []
