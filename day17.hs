{-# LANGUAGE MultiWayIf #-}
import Aoc
import Data.Maybe

main = do
  [minX,maxX,minY,maxY] <- getInts
  let hit (x,y,vx,vy,topY) =
        if | y < minY -> Nothing
           | x > maxX -> Nothing
           | vx == 0 && x < minX -> Nothing
           | y <= maxY && x >= minX && x <= maxX -> Just topY
           | otherwise -> hit (x+vx, y+vy, vx-signum vx, vy-1, max topY (y+vy))
  let hits = catMaybes [hit (0,0,vx,vy,0) | vx <- [0..maxX], vy <- [minY.. -minY]]
  print $ maximum hits
  print $ length hits
