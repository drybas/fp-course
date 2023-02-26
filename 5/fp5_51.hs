import System.IO

prompt = do 
    putStr "Name: "
    hFlush stdout
    getLine

main = do 
    putStrLn "What is your name?"
    name <- prompt
    if name == "" then main else putStrLn $ "Hi, " ++ name ++ "!" 