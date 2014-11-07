import Data.List

solveRPN :: (Floating a, Read a) => String -> a
solveRPN = head . foldl ff [] . words
    where ff (x:y:zs) "*" = (x * y):zs
          ff (x:y:zs) "+" = (x + y):zs
          ff (x:y:zs) "-" = (x - y):zs
          ff (x:y:zs) "/" = (x / y):zs
          ff (x:y:zs) "^" = (x ** y):zs
          ff (x:zs) "ln" = (log x):zs
          ff xs "sum" = [sum xs]
          ff xs numberString = (read numberString):xs
