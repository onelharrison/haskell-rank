solve :: [Int] -> [Int]
solve (s:t:a:b:m:_:rest) = [aCount, oCount]
  where aCount = length $ filter (\x -> s <= x && x <= t) $ map (\x -> x + a) $ take m rest
        oCount = length $ filter (\x -> s <= x && x <= t) $ map (\x -> x + b) $ drop m rest

main = interact $ unlines . map show . solve . map read . words
