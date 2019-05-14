import Data.List

conv :: Char -> Int
conv 'U' =  1
conv 'D' = -1

solve :: String -> Int
solve = length
        . filter (\xs -> head xs < 0)
        . groupBy (\x y -> x /= 0 && y /= 0)
        . scanl (+) 0
        . map conv

main :: IO ()
main = interact $ show . solve . head . tail . words
