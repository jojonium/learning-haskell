import System.Random
import System.Environment
import qualified Data.Text as T

-- parses command line arguments in the form xdy, where x is the number of dice
-- to roll and y is the number of sides on those dice. For each argument, rolls
-- the appropriate dice and prints the output as a line on stdout
main = 
  do gen  <- getStdGen
     args <- getArgs
     if length args > 0
       then let nums = map ((flip roll) gen . splitDice) args
            in mapM_ print nums
       else putStrLn usage

splitDice :: String -> (Int, Int)
splitDice s = 
  let t = T.pack s
      (a:b:_) = map (read . T.unpack) (T.splitOn (T.singleton 'd') t) :: [Int]
  in  (a, b)

-- rolls xdy and returns a list of the results
roll :: (Int, Int) -> StdGen -> [Int]
roll (x, y) gen = take x $ randomRs (1, y) gen

usage :: String
usage = "What dice to roll?\ne.g. dice 1d3, dice 4d10 6d8"

