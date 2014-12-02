import Control.Monad.Writer
import Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, log') = f x in (y, log `mappend` log')

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell [show x ++ " is less than 4!"]
        return True
    | otherwise = do
        tell [show x ++ " is gone!"]
        return False
