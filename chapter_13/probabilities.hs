import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show)

instance Functor Prob a where
    fmap f (Prob xs) :: Prob $ map (f . fst) 
    -- TODO
