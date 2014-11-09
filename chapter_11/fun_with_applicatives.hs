myFunction :: IO String
myFunction = (++) <$> getLine <*> getLine

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
