main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    unlines (filter (\line -> length line < 10) (lines input))
