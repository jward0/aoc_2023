import Data.Char
import Data.List

main = do
    input <- getContents
    let foo = sum (map valueFinder (lines input))
    putStrLn (show foo)
    let bar = sum (map valueFinder (map wordsToDigits (lines input)))
    putStrLn (show bar)

valueFinder :: String -> Int
valueFinder input = read [head digits, last digits]
    where digits = filter isDigit input

firstDigit :: String -> Int 
firstDigit [] = 0
firstDigit (x:xs)
    | isDigit x = read [x] :: Int
    | otherwise = firstDigit xs

wordsToDigits :: String -> String
wordsToDigits input = reverse $ foldl go "" $ tails input
    where go acc [] = acc
          go acc xs
              | "one"   `isPrefixOf` xs = '1' : acc
              | "two"   `isPrefixOf` xs = '2' : acc
              | "three" `isPrefixOf` xs = '3' : acc
              | "four"  `isPrefixOf` xs = '4' : acc
              | "five"  `isPrefixOf` xs = '5' : acc
              | "six"   `isPrefixOf` xs = '6' : acc
              | "seven" `isPrefixOf` xs = '7' : acc
              | "eight" `isPrefixOf` xs = '8' : acc
              | "nine"  `isPrefixOf` xs = '9' : acc
              | isDigit (head xs) = (head xs) : acc
              | otherwise = acc

-- valueFinder :: String -> Int
-- valueFinder input = 10*sum (map firstDigit (lines input)) + sum (map (firstDigit.reverse)  (lines input))
