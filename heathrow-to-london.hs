data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show) 
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30 -- test data
                   , Section 5  90 20
                   , Section 40 2  25
                   , Section 10 8  0
                   ]


main =
  do contents <- getContents -- reads road data from stdin
     let threes     = groupsOf 3 (map read $ lines contents)
         roadSystem = map (\[a, b, c] -> Section a b c) threes
         bestPath   = optimalPath roadSystem
         pathString = concat $ map (show . fst) bestPath
         pathPrice  = sum $ map snd bestPath
     putStrLn $ "The best path to take is: " ++ pathString
     putStrLn $ "The price of the best path is: " ++ show pathPrice


optimalPath :: RoadSystem -> Path
optimalPath roads = 
  let (pathA, pathB, priceA, priceB) = foldl (roadStep) ([], [], 0, 0) roads
  in  reverse (if priceA <= priceB then pathA else pathB)


-- completes one step of the problem, taking the two existing paths and costs
-- and returning the new optimal path and price to the next intersection on A
-- and B respectively
roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, priceA, priceB) (Section a b c) = 
  let straightToA = priceA + a
      straightToB = priceB + b
      crossToA    = priceB + b + c
      crossToB    = priceA + a + c
      newPathToA  = if straightToA <= crossToA
                       then (A,a):pathA
                       else (C,c):(B,b):pathB
      newPriceA   = min straightToA crossToA
      newPathToB  = if straightToB <= crossToB
                       then (B,b):pathB
                       else (C,c):(A,a):pathA
      newPriceB   = min straightToB crossToB
  in  (newPathToA, newPathToB, newPriceA, newPriceB)


-- splits up lists into sublists of size n
groupsOf :: (Integral a) => Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
