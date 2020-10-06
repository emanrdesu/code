
--    *
--   ***
--  *****
-- *******

pyramid n = zipWith slab [n-1,n-2..0] [1,3..]
  where slab w s = concat $ replicate w " " ++ replicate s "*"

pio = mapM_ putStrLn . pyramid
