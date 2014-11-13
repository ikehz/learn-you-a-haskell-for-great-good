import Control.Monad

type KnightPosition = (Int, Int)

moveKnight :: KnightPosition -> [KnightPosition]
moveKnight (c, r) = do
    (c', r') <- [(c+1, r+2), (c+2, r+1), (c-1, r+2), (c-2, r+1),
                 (c+1, r-2), (c+2, r-1), (c-1, r-2), (c-2, r-1)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPosition -> [KnightPosition]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPosition -> KnightPosition -> Bool
canReachIn3 start end = end `elem` in3 start
