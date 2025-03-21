
fib :: Int -> Int
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fib $ n - 1) + (fib $ n - 2)

-- Naive approach
fibs :: [Int]
fibs = map fib [0..]

-- Optimized
fibs' :: [Int]
fibs' = 0 : 1 : helper fibs' (tail fibs')
  where
    helper (a:as) (b:bs) =
        a + b : helper as bs

smallFibs :: [Int]
smallFibs = takeWhile (< 1000) fibs'

main :: IO ()
main = print (fibs' !! 100)
    