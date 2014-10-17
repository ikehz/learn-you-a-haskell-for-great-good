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
        topArea  = pi * r ^ 2
    in sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty."
                                                [_] -> "a singleton list."
                                                [_,_] -> "a doubleton list."
                                                _ -> "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "Can't find the maximum of an empty list"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

repeat' :: a -> [a]
repeat' a = a:repeat' a

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [ a | a <- xs, a <= x ]
        biggerSorted = quicksort [ a | a <- xs, a > x ]
    in  smallerSorted ++ [x] ++ biggerSorted

-- zipWith takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip simply takes a function and returns a function that is like our original function, only the first two arguments are flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- map takes a function and a list and applies that function to every element in the list, producing a new list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter is a function that takes a predicate and a list and then returns the list of elements that satisfy the predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = (quicksort lt) ++ [x] ++ (quicksort geq)
    where lt  = filter (< x) xs
          geq = filter (>= x) xs

-- takeWhile takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

-- Collatz sequence
collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n  = n : collatz (3 * n + 1)
