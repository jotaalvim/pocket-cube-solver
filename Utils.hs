module Utils where

--- copied from 
-- https://hackage-content.haskell.org/package/Agda-2.8.0/docs/src/Agda.Utils.List.html#distinct

--   O(n²) in the worst case @distinct xs == True@.
distinct :: Eq a => [a] -> Bool
distinct []     = True
distinct (x:xs) = x `notElem` xs && distinct xs