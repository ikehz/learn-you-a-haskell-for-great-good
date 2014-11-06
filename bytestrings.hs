import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
    (sourceFName:targetFName:_) <- getArgs
    copyFile sourceFName targetFName

copyFile :: FilePath -> FilePath -> IO ()
copyFile sourceFName targetFName = do
    contents <- B.readFile sourceFName
    B.writeFile targetFName contents
