import Control.Monad

getList :: Read a => IO [a]
getList = getLine >>= return . map read . words

solve :: Int -> [Int] -> Int -> Maybe Int
solve k bill b
  | b > actualCost = Just (b - actualCost)
  | otherwise      = Nothing
  where actualCost = (sum bill - bill !! k) `div` 2

main :: IO ()
main = do
  [[_, k], bill, [b]] <- replicateM 3 getList
  putStrLn $ maybe "Bon Appetit" show $ solve k bill b
