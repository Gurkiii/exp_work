module ListFuncs (append, get)where

--appends lists cleaner and more elegant than the ++ function
append :: [a] -> [a] -> [a]
append as bs = foldr (:) bs as 

--gets the n-th element from list as
get :: Integer -> [a] -> a
get 1 as = head as 
get n as = if n < 1
    then get (-n) as
    else get (n-1) (tail as)

