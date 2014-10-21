import System.IO
import System.Directory
import Data.List

main = do
    -- open files
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "todo"
    -- get contents
    contents <- hGetContents handle
    let todos = lines contents
        numberedTodos = zipWith (\n t-> show n ++ " - " ++ t) [0..] todos
    -- print and ask for removal
    putStrLn "Here is your to-do list:"
    mapM putStrLn numberedTodos
    putStrLn "Which would you like to remove?"
    numberString <- getLine
    -- remove
    let toRemove = read numberString
        newTodos = delete (todos !! toRemove) todos
    -- write new file
    hPutStr tempHandle $ unlines newTodos
    -- close and rename
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
