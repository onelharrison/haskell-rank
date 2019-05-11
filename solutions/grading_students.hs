round5 :: Int -> Int
round5 x
  | (x >= 38) && ((nextMult5 - x) < 3) = nextMult5
  | otherwise = x
  where nextMult5 = x + (5 - (x `mod` 5))

main = interact $ unlines . map (show . round5 . read) . tail . words
