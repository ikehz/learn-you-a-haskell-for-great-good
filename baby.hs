doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- a comprehension that replaces each odd number greater than 10 with "BANG!" and each odd number that's less than 10 with "BOOM!"
boomBangs :: Integral a => [a] -> [String]
boomBangs xs = [ if x > 10 then "BANG!" else "BOOM!" | x <- xs, odd x ]

prepositionalPhrases :: [String] -> [String] -> [String]
prepositionalPhrases prepositions nouns = [ p ++ " " ++ n | p <- prepositions, n <- nouns ]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- a function that takes a string and removes everything except uppercase letters from it
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

-- remove all odd numbers without flattening the list
removeOdds :: (Integral a) => [[a]] -> [[a]]
removeOdds xxs = [ [ x | x <- xs, even x ] | xs <- xxs ]

-- which right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24?

triangle24 :: [(Int, Int, Int)]
triangle24 = [ (a,b,c) | (a,b,c) <- [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2], a + b + c == 24 ]

lucky :: (Integral a) => a -> String
lucky 7 = "Win!"
lucky x = "Too bad."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

charName :: Char -> String
charName 'a' = "Al"
charName 'b' = "Bob"
charName 'c' = "Chris"

-- a function that takes two vectors in a 2D space (that are in the form of pairs) and adds them together
addVectors :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- our own implementation of the head function
head' :: [a] -> a
head' [] = error "Can't find the head of a headless list"
head' (x:_) = x

-- our own implementation of the length function, with recursion
length'' :: [a] -> Int
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

first :: String -> String
first "" = "Oops, empty!"
first all@(c:_) = "The first character of " ++ all ++ " is " ++ [c] ++ "."

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    where bmi = weight / height ^2

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a < b     = LT
    | a > b     = GT
    | otherwise = EQ

-- another fairly trivial function where we get a first and a last name and give someone back their initials
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- a function that takes a list of weight-height pairs and returns a list of BMIs
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- a function that gives us a cylinder's surface area based on its height and radius
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty."
                                                [_] -> "a singleton list."
                                                [_,_] -> "a doubleton list."
                                                _ -> "a longer list."
