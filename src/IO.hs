module IO where


getLineHelper :: [Char] -> IO [Char]
getLineHelper container = getChar >>= \r -> return (container ++ [r])

-- Implement reading line from standard input. 
-- Use getChar to read a single character.
myGetLine' :: [Char] -> IO String
myGetLine' state = do
    array <- getLineHelper state
    if last array == '\n'
        then return array
    else getLineHelper array >>= myGetLine'

myGetLine = myGetLine' []

-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO ()
helloUser = do
    putStrLn "What is your name?"
    name <- myGetLine
    putStrLn ("Hello, " ++ name)


-- Use interact in helloUser.
helloUser' :: IO ()
helloUser' = do
    putStrLn "What is your name?"
    interact (\name -> "Hello, " ++ name)
