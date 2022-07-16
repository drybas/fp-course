import System.IO
import System.Directory  
import Data.List;

prompt' = do 
    putStr "Substring: "
    hFlush stdout
    getLine

removeFile' :: FilePath -> IO ()
removeFile' fn = do 
    putStrLn $ "Removing file: " ++ fn
    removeFile fn

main = do 
    mask <- prompt'
    if mask == "" then putStrLn "Canceled" else do
        b <- getDirectoryContents "."
        mapM_ removeFile' (filter (isInfixOf mask) b) 
