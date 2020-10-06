
import Data.List

myList = [5,3,2,3,5,1]

ssort [] = []
ssort xs = min : ssort (delete min xs)
  where min = minimum xs

qsort [] = []
qsort (y:ys) = (qsort min) ++ [y] ++ (qsort max)
  where min = (filter (<y) ys)
        max = (filter (>y) ys)

msort [] = []
msort [x] = [x]
msort xs = let h = div (length xs) 2
           in merge (msort $ take h xs) (msort $ drop h xs) where
  merge = curry $ unfoldr ca where
    ca ([], []) = Nothing
    ca ([], y:ys) = Just (y, ([], ys))
    ca (x:xs, []) = Just (x, (xs, []))
    ca (x:xs, y:ys)
      | x <= y = Just (x, (xs, y:ys))
      | x >  y = Just (y, (x:xs, ys))
