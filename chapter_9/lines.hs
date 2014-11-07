import System.Environment
import System.IO
import System.IO.Error

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ (show $ length $ lines contents) ++ " lines."

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just fileName -> putStrLn $ fileName ++ " doesn't exist, sorry."
                                 Nothing -> putStrLn "File doesn't exist, but we're not sure which."
    | otherwise = ioError e
