import Aoc
import Data.List

foldAlong 'x' n [x,y] = [if x > n then n-(x-n) else x, y]
foldAlong 'y' n [x,y] = [x, if y > n then n-(y-n) else y]
[x, y] % s = let c:_:n = last (words s) in foldAlong c (read n) [x, y]

main = do
  [p, q] <- getParagraphs
  let xys = map readInts p
  print $ length $ nub [xy % head q | xy <- xys]
  let points = nub [foldl (%) xy q | xy <- xys]
  let [minX, minY] = foldl1 (zipWith min) points
  let [maxX, maxY] = foldl1 (zipWith max) points
  mapM putStrLn [[if [x,y] `elem` points then '█' else '·' | x<-[minX..maxX]] | y<-[minY..maxY]]
