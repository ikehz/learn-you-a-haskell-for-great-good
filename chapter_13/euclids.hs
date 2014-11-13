import Control.Monad.Writer
import Data.Monoid

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
    | b == 0 = do
        tell $ toDiffList ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList ([]++)
    (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell $ toDiffList ["0"]
finalCountDown n = do
    finalCountDown (n-1)
    tell $ toDiffList [show n]

finalCountDownSlow :: Int -> Writer [String] ()
finalCountDownSlow 0 = do
    tell $ ["0"]
finalCountDownSlow n = do
    finalCountDownSlow (n-1)
    tell $ [show n]
