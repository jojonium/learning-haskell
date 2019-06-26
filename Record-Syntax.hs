-- fname, lname, age, height, phone, favorite flavor of ice cream
data Person = Person { firstName :: String 
                     , lastName  :: String 
                     , age       :: Int
                     , height    :: Float
                     , phoneNum  :: String
                     , flavor    :: String
                     } deriving (Show)

data Car = Car { company :: String
               , model   :: String
               , year    :: Int
               } deriving (Show)

data Vector a = Vector a a a deriving (Show)

-- the Vector type constructor takes one argument, while the Vector value
-- constructor takes three
vPlus :: (Num t) => Vector t -> Vector t -> Vector t
vPlus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

