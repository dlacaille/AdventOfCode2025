import Data.Char (toUpper)

-- Adds an exclamation mark to the end of a string
exclaim :: String -> String
exclaim str = str ++ "!"

-- Capitalizes the first letter of a string
capitalize :: String -> String
capitalize (head : tail) = toUpper head : tail
capitalize [] = []

-- Capitalizes the first letter of each word in a sentence
capitalizeWords :: String -> String
capitalizeWords sentence = unwords $ map capitalize $ words sentence

main :: IO ()
main = putStrLn $ exclaim $ capitalizeWords "hello world"
