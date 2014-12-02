import Data.List
import Control.Monad
import Text.Read

solveRPN :: (Floating a, Read a) => String -> Maybe a
solveRPN s = do
    [result] <- foldM ff [] $ words s
    return result

ff :: (Floating a, Read a) => [a] -> String -> Maybe [a]
ff (x:y:zs) "*" = return ((x * y):zs)
ff (x:y:zs) "+" = return ((x + y):zs)
ff (x:y:zs) "-" = return ((x - y):zs)
ff (x:y:zs) "/" = return ((x / y):zs)
ff (x:y:zs) "^" = return ((x ** y):zs)
ff (x:zs) "ln" = return ((log x):zs)
ff xs "sum" = return [sum xs]
ff xs numberString = liftM (:xs) (readMaybe numberString)
