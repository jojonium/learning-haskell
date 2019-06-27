-- reverse polish notation calculator

solveRPN :: String -> Float
solveRPN = head . foldl (apply) [] . words
    where apply (x:y:ys) "*"  = (x * y):ys
          apply (x:y:ys) "/"  = (x / y):ys
          apply (x:y:ys) "+"  = (x + y):ys
          apply (x:y:ys) "-"  = (x - y):ys
          apply (x:y:ys) "^"  = (x ** y):ys
          apply (x:y:ys) "ln" = log x:ys
          apply xs "sum"      = [sum xs]
          apply xs numString  = read numString:xs
