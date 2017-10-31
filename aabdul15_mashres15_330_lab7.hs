import System.Random

reply:: [String]
reply = ["It is certain.", "You may rely on it.", "Outlook not so good.", "Reply hazy try again."]

eightBall:: IO ()
eightBall = do
    putStr: "Please enter your question: "
    question <- getLine
    n <- randomRIO(0,2) :: IO Int
    putStr "\n-> "
    putStrLn (reply!! (fromIntegral n))
    putStrLn ""
    

