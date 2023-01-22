--Quicksort
--recursively going through all the units inside the list,
--split into 2 lists containing either larger or smaller than x

--filter takes conditions as a Int->Bool
--usually (> takes 2 parameters) as  Int->Int->Bool -- curried functions
qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) =  qs(filter (< x) xs) ++ [x] ++ qs(filter (>=x) xs)
