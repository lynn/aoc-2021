import Aoc
import Data.Maybe

main = do
  m <- getLines
  let sy = length m
  let sx = length (m!!0)
  let at (x,y) | 0 <= x, x < sx, 0 <= y, y < sy = Just (m!!y!!x)
               | otherwise = Nothing
  let neighbors (x,y) = catMaybes $ at <$> [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  print $ sum [read[h]+1 | y <- [0..sy-1], x <- [0..sx-1],
    let h = m!!y!!x, all (>h) $ neighbors (x,y)]
