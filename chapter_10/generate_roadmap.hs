import System.Environment
import System.Random

roadsInSection = 3
minRoad = 0
maxRoad = 100

main = do
    (numSectionsString:fName:_) <- getArgs
    let numSections = read numSectionsString :: Int
    -- get a random number generator
    gen <- getStdGen
    let sectionList = take (numSections * roadsInSection) $ randomRs (minRoad, maxRoad) gen :: [Int]
    writeFile fName (unlines $ map show sectionList)
