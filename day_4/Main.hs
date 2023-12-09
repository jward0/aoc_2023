import Text.Regex.Posix

main = do 
    input <- getContents
    putStrLn $ show (part1 input)
    putStrLn $ show (part2 input)

countMatches :: String -> Int
countMatches line = length $ filter(\i -> i) (map (\x -> elem x winners) own)
    where [winners, own] = map words [fst spl, snd spl]  
          spl = break ('|'==) line

getCardNumber :: String -> Int
getCardNumber line = read (init ((words line) !! 1))::Int


-- counts is incremented by n(cards considered) at all indices from fst cardMatches + 1 to snd cardMatches + 1
addWinnings :: [Int] -> (Int, Int) -> [Int]
addWinnings counts cardMatches = zipWith (+) counts newWinnings
    where nCards = counts !! (fst cardMatches)
          onesIndices = [(fst cardMatches)+1..(fst cardMatches) + (snd cardMatches)]
          newWinnings = [if x `elem` onesIndices then nCards else 0 | x <- [0..(length counts)]]

listAbuse :: [(Int, Int)] -> [Int]
listAbuse cardMatches = foldl addWinnings initialCounts cardMatches
    where initialCounts = take (length cardMatches) (repeat 1)

part1 :: String -> Int
part1 input = sum $ map getScore (map countMatches (lines input))
    where getScore i 
            | i == 0    = 0
            | otherwise = 2^(i-1)

part2 :: String -> Int
part2 input = sum $ listAbuse (zip [0..] (map countMatches (lines input)))
