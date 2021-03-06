-- x1 < x2
solve :: [Int] -> String
solve [x1, v1, x2, v2]
  | (v1 > v2) && (((x1 - x2) `mod` (v2 - v1)) == 0) = "YES"
  | otherwise = "NO"

main :: IO ()
main = interact $ solve . map read . words
