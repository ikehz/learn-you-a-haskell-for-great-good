data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
