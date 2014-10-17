-- XXX I can't tell how foldl and foldr are implemented in Haskell, especially as they pertain to infinite lists.

-- foldl folds the list up from the left side
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' f acc (y:ys) = foldl' f (f acc y) ys

-- foldr folds the list up from the right side
-- XXX this fails on map' ... see below
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc xs = foldr' f (f (last xs) acc) (init xs)

-- foldr folds the list up from the right side
-- XXX this works on map' ... see below
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ acc [] = acc
foldr'' f acc (x:xs) = f x (foldr'' f acc xs)

-- elem checks whether a value is part of a list
-- XXX this doesn't ever terminate on infinite lists, even if the element is found
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldl (\ acc  y -> acc || x == y) False

-- map takes a function and a list and applies that function to every element in the list, producing a new list
-- XXX this somehow works on infinite lists, (but not if it uses foldr')
map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr (\ y acc -> f y : acc) [] -- works
-- map' f = foldr' (\ y acc -> f y : acc) [] -- fails
map' f = foldr'' (\ y acc -> f y : acc) [] -- works

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: Num a => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\ x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldl1 (\ acc _ -> acc)

last' :: [a] -> a
last' = foldr1 (\ _ acc -> acc)
