module Quicksort (quicksort) where

quicksort :: (Ord a) => [a] -> [a]
quicksort as
    | l > 2 = append (append (quicksort bs) cs) ( quicksort ds)
    | l == 2 = [minimum as, maximum as]
    | otherwise = as
    where piv = pivot as
          bs = [ x | x <- as, x < piv]
          cs = [ x | x <- as, x == piv]
          ds = [ x | x <- as, x > piv]
          l = length as

--determines the pivot element to use for quicksort by choosing the middle element from the list
pivot :: [a] -> a
pivot as = get (toInteger (div (length as +1) 2)) as

--test: quicksort [2,45,8,3,2,45,56,6,8,7,4,23,2,5,65,7,456,456,345,56,34,758,69,123,5,7,6,3,47,86,47,12,4,3,6]


--from ListFuncs because import doesn't seem to work on the IntelliJ Haskell plugin

--appends lists cleaner and more elegant than the ++ function
append :: [a] -> [a] -> [a]
append as bs = foldr (:) bs as 

--gets the n-th element from list as
get :: Integer -> [a] -> a
get 1 as = head as 
get n as = if n < 1
    then get (-n) as
    else get (n-1) (tail as)
