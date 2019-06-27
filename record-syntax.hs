import qualified Data.Map as Map

-- fname, lname, age, height, phone, favorite flavor of ice cream
data Person = Person { firstName :: String 
                     , lastName  :: String 
                     , age       :: Int
                     , height    :: Float
                     , phoneNum  :: String
                     , flavor    :: String
                     } deriving (Show, Eq, Read)

data Car = Car { company :: String
               , model   :: String
               , year    :: Int
               } deriving (Show)

data Vector a = Vector a a a deriving (Show)

-- the Vector type constructor takes one argument, while the Vector value
-- constructor takes three
vPlus :: (Num t) => Vector t -> Vector t -> Vector t
vPlus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

createMustang :: Car
createMustang = Car {model = "Mustang", year = 1967, company = "Ford"}


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
  [ (100, (Taken, "ZD391"))
  , (101, (Free , "39BJE"))
  , (102, (Taken, "B938H"))
  , (103, (Free,  "39BBJ"))
  , (104, (Free,  "9302Q"))
  ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                             then Right code
                             else Left $ "Locker " ++ show lockerNumber ++ " is taken!"


-- Binary Search Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeRoot :: a -> Tree a
treeRoot x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = treeRoot x
treeInsert x (Node r left right)
  | x == r = Node x left right
  | x < r  = Node r (treeInsert x left) right
  | x > r  = Node r left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node r left right)
  | x == r = True
  | x < r  = treeElem x left
  | x > r  = treeElem x right

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList xs = foldr treeInsert EmptyTree xs

-- make it an instance of Functor so it can be mapped
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f leftsub) (fmap f rightsub)
