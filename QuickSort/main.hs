
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
  where
    lesser = filter (< x) xs
    greater = filter (>= x) xs

main :: IO ()
main = print $ quicksort ['a','v','c','r','s']--[5,3,1,2,9,6,4]