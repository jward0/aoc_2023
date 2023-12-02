import Text.Regex.Posix

main = do
    input <- getContents
    putStrLn $ show (part1 input)
    putStrLn $ show (part2 input)


numbers :: String -> [Int]
numbers s = map read (getAllTextMatches (s =~ "[[:digit:]]+") :: [String])


colourLimits :: String -> [Int]
colourLimits s = map getLimits (getAllTextMatches (s =~ "[a-z]+") :: [String])
    where getLimits colour
              | colour == "red"   = 12
              | colour == "green" = 13
              | colour == "blue"  = 14
              | otherwise         = 0


scoreLine :: String -> Int
scoreLine l 
    | valid l == True = head (numbers l)
    | otherwise       = 0
    where valid l = all(\(x, y) -> x <= y) (zip (tail (numbers l)) (tail (colourLimits l)))
 

minColour :: String -> Int -> Int
minColour input cLim  = maximum (map fst (filter(\x -> (snd x) == cLim) (zip nums lims)))
    where nums = tail (numbers input)
          lims = tail (colourLimits input)


getPower :: String -> Int
getPower l = (minColour l  12) * (minColour l 13) * (minColour l 14)

 
part1 :: String -> Int
part1 input = sum (map scoreLine (lines input))


part2 :: String -> Int
part2 input = sum (map getPower (lines input))
