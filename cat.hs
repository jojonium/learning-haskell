import System.Environment

-- cat - concatenates files and prints on the standard output.
-- Usage: cat [FILE]...
-- With no FILE the standard input is read
main = 
  do args <- getArgs
     if length args > 0 
       then mapM_ catOne args
       else catStdIn

catOne :: FilePath -> IO ()
catOne name = 
  do contents <- readFile name
     putStr contents

catStdIn :: IO ()
catStdIn = 
  do line <- getLine
     putStrLn line
     catStdIn
