data Section = Section { a :: Int, b :: Int, c :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

optimalPath :: RoadSystem -> Path
optimalPath rs =
    let (pathA, pathB) = foldl ff ([],[]) rs
    in if (sum $ map snd pathA) <= (sum $ map snd pathB)
       then reverse pathA
       else reverse pathB

ff :: (Path, Path) -> Section -> (Path, Path)
ff (pathA, pathB) (Section costA costB costC) =
    let pricePathA = sum $ map snd pathA
        pricePathB = sum $ map snd pathB
        priceForwardToA = pricePathA + costA
        priceCrossToA = pricePathB + costB + costC
        priceForwardToB = pricePathB + costB
        priceCrossToB = pricePathA + costA + costC
        newPathToA = if priceForwardToA <= priceCrossToA
                     then (A, costA):pathA
                     else (C, costC):(B, costB):pathB
        newPathToB = if priceForwardToB <= priceCrossToB
                     then (B, costB):pathB
                     else (C, costC):(A, costA):pathA
    in (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = (take n xs):(groupsOf n $ drop n xs)

main = do
    contents <- getContents
    let threes = groupsOf 3 $ (map read $ lines contents)
        rs = map (\[a,b,c] -> (Section a b c)) threes
        path = optimalPath rs
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The shortest path is:" ++ pathString
    putStrLn $ "The cost is:" ++ show pathPrice
