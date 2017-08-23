module Utils (group) where

-- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"
