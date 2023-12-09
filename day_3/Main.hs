import Text.Regex.Posix

main = do
    input <- getContents
    putStrLn $ show (part1 input)


isNearSymbol :: [String] -> (Int, Int) ->  Bool
isNearSymbol schematic (i, j) = any(\x -> x == True) $ map (\(x, y) -> checkIndex schematic (x, y)) (expandCoords (i,j))
    where expandCoords (i, j) =  [(i-1, j-1), (i, j-1), (i+1, j-1), (i-1, j), (i, j), (i+1, j), (i-1, j+1), (i, j+1), (i+1, j+1)]   
    -- where expandCoords (i, j) = map (\(x, y) -> (x+i, y+j)) (range ((-1, -1), (1, 1)))

checkIndex :: [String] -> (Int, Int) -> Bool
checkIndex array (rowIndex, columnIndex) = 
    case array !!? rowIndex of
        Just row -> case row !!? columnIndex of
            Just '.'  -> False
            Just '0'  -> False
            Just '1'  -> False
            Just '2'  -> False
            Just '3'  -> False
            Just '4'  -> False
            Just '5'  -> False
            Just '6'  -> False
            Just '7'  -> False
            Just '8'  -> False
            Just '9'  -> False
            Nothing   -> False
            otherwise -> True
        Nothing   -> False
    where
        (!!?) :: [a] -> Int -> Maybe a
        list !!? index
            | index < 0 || index >= length list = Nothing
            | otherwise                         = Just (list !! index)

checkIndexPt2 :: [String] -> (Int, Int) -> Bool
checkIndexPt2 array (rowIndex, columnIndex) = 
    case array !!? rowIndex of
        Just row -> case row !!? columnIndex of
            Just '*'  -> True
            otherwise -> False
        Nothing   -> False
    where
        (!!?) :: [a] -> Int -> Maybe a
        list !!? index
            | index < 0 || index >= length list = Nothing
            | otherwise                         = Just (list !! index)

transformCoords :: (Int, Int) -> Int -> [(Int, Int)]
transformCoords (start, length) row = map (\x -> (row, x)) [start..start+length-1]  

checkValue :: [String] -> String -> [(Int, Int)] -> Int
checkValue schematic intStr coords 
    | truth == True = read intStr
    | otherwise     = 0
    where truth = any (\x -> x == True) (map (\x -> isNearSymbol schematic x) coords)  

sumLine :: Int -> String -> [String] -> Int
sumLine row l schematic = sum $ map (\(v, c) -> checkValue schematic v c) zl
    where zl = zip (getAllTextMatches (l =~ "[[:digit:]]+") :: [String]) (map (\x -> transformCoords x row) (getAllMatches (l =~ "[[:digit:]]+") :: [(Int, Int)]))

part1 :: String -> Int
part1 input = sum $ map (\(x, y) -> sumLine x y (lines input)) (zip [0..]  (lines input))    
