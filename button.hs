
import Data.List
import Data.Char

layout :: [(Int, String)]
layout = [ (1, "1"), (2, "abc2"), (3, "def3"),
           (4, "ghi4"), (5, "jkl5"), (6, "mno6"),
           (7, "pqrs7"), (8, "tuv8"), (9, "wxyz9"),
           (-1, "\a*"), (0, "\b 0"), (-2, ".,#")
         ]

unsymbol :: Char -> Int
unsymbol '*' = -1
unsymbol '#' = -2
unsymbol n = read [n]

press :: String -> String
press = capitalize . filter (/='\b') . map letter . group . map unsymbol
  where capitalize [] = []
        capitalize [x] = [x]
        capitalize (x:y:zs) = if (x == '\a')
                              then toUpper y : capitalize zs
                              else x : capitalize (y:zs)
        letter x@(n:_) = head . drop (length x - 1) $ cycle l
          where Just l = lookup n layout

symbol :: Int -> Char
symbol (-1) = '*'
symbol (-2) = '#'
symbol n = head (show n)

unpress :: String -> String
unpress = concatMap up . groupBy (\x y -> str x == str y)
  where str c = let Just (_, s) = find (elem (toLower c) . snd) layout in s
        up [c] | isUpper c = '*' : up [toLower c]
               | otherwise = let Just (b, s) = find (elem c . snd) layout
                                 Just i = findIndex (==c) s
                             in map symbol $ replicate (i+1) b
        up mc = concat . intersperse "0" $ map (up . (:[])) mc
