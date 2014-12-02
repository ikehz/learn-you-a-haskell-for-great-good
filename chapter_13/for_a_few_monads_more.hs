import Control.Monad

powerSet :: [a] -> [[a]]
powerSet = filterM (\x -> [True, False])

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)
