import Aoc
import Data.List
import Data.Maybe

stabilize f x = until (\x -> f x == x) f x

main = do
  m <- getLines
  let sy = length m
  let sx = length (m!!0)
  let at (x,y) | 0 <= x, x < sx, 0 <= y, y < sy = Just (m!!y!!x)
               | otherwise = Nothing
  let nc (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  let neighbors (x,y) = catMaybes $ at <$> nc (x,y)

  let lows = [((x,y),read[h]) | y <- [0..sy-1], x <- [0..sx-1],
              let h = m!!y!!x, all (>h) $ neighbors (x,y)]

  let risk = sum [h+1 | (_,h) <- lows]
  print risk

  let basins = [[p] | (p,_) <- lows]
  let grow basin = nub $ basin ++ [p' | p <- basin, p' <- nc p, Just h <- [at p'], h < '9']
  let final = stabilize (map grow) basins
  print $ product $ take 3 $ reverse $ sort $ map length final

