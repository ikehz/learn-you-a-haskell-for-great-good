import Control.Monad
import System.IO
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    hFlush stdout
    l <- getLine
    putStrLn $ map toUpper l
