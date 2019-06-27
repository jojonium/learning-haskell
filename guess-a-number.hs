import System.Random
import Control.Monad(when)

main = do gen <- getStdGen
          askForNum gen

askForNum :: StdGen -> IO ()
askForNum gen =
    do let (randomNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
       putStr "Which number in the range [1, 10] am I thinking of? "
       input <- getLine
       when (not $ null input) $
            do let inNumber = read input :: Int
               if randomNumber == inNumber
                  then putStrLn "You are correct!"
                  else putStrLn $ "Sorry, it was " ++ show randomNumber
               askForNum newGen
