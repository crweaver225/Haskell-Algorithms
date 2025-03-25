module Sorts.BubbleSort where

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort lst
    | bubblePass lst == lst = lst
    | otherwise = bubbleSort $ bubblePass lst


bubblePass :: (Ord a) => [a] -> [a]
bubblePass [] = []
bubblePass [x] = [x]
bubblePass (x1:x2:xs)
    | x1 > x2 = x2 : bubblePass (x1:xs)
    | otherwise = x1 : bubblePass (x2:xs)