import Aoc
import Data.Graph.AStar (aStar)
import qualified Data.HashSet as H

dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- you want me to implement A*? the thing that killed the minotaur?
risk maze =
  let maxY = length maze - 1
      maxX = length (head maze) - 1
      goal = (maxX,maxY)
      cost (x,y) = maze!!y!!x
      ok (x,y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
      nc (x,y) = H.fromList $ filter ok [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
      Just path = aStar nc (const cost) (dist goal) (==goal) (0,0)
  in sum $ map cost path

main = do
  ls <- getLines
  let maze = map (map (read . pure)) ls
  print $ risk maze
  let big = [[mod (n+x+y-1) 9 + 1 | x <- [0..4], n <- row]
                                  | y <- [0..4], row <- maze]
  print $ risk big

