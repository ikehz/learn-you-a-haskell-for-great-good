import Control.Monad
import Data.List

type KnightPosition = (Int, Int)

moveKnight :: KnightPosition -> [KnightPosition]
moveKnight (c, r) = do
    (c', r') <- [(c+1, r+2), (c+2, r+1), (c-1, r+2), (c-2, r+1),
                 (c+1, r-2), (c+2, r-1), (c-1, r-2), (c-2, r-1)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPosition -> [KnightPosition]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

inMany :: Int -> KnightPosition -> [KnightPosition]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachInMany :: Int -> KnightPosition -> KnightPosition -> Bool
canReachInMany x start end = end `elem` inMany x start
