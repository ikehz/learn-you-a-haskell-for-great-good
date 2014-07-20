doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- a comprehension that replaces each odd number greater than 10 with "BANG!" and each odd number that's less than 10 with "BOOM!"
boomBangs xs = [ if x > 10 then "BANG!" else "BOOM!" | x <- xs, odd x ]

prepositionalPhrases prepositions nouns = [ p ++ " " ++ n | p <- prepositions, n <- nouns ]

length' xs = sum [1 | _ <- xs]

-- a function that takes a string and removes everything except uppercase letters from it
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

-- remove all odd numbers without flattening the list
removeOdds xxs = [ [ x | x <- xs, even x ] | xs <- xxs ]

-- which right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24?

triangle24 = [ (a,b,c) | (a,b,c) <- [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2], a + b + c == 24 ]
