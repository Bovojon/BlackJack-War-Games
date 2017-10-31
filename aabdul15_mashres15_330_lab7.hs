import System.Random

-- Coding Assignment 7.1: (1 Point)
helloWorld = putStrLn "Hello, world"

-- Coding Assignment 7.2: (1 Point)
helloName = do 
    putStrLn "Please enter your name."
    name <- getLine
    putStrLn ("Hello, " ++ name)

--Coding Assignment 7.3: (5 Points)

reply:: [String]
reply = ["It is certain.", "You may rely on it.", "Outlook not so good.", "Reply hazy try again."]

eightBall:: IO ()
eightBall = do
    putStr "Please enter your question: "
    question <- getLine
    n <- randomRIO(0,2) :: IO Int
    putStr "\n-> "
    putStrLn (reply!! n)
    putStrLn ""
    

