import Aoc
import qualified Data.Map as M

main = do
  grid <- getLines
  let height = length grid
  let width = length (grid!!0)
  let m = M.fromList [((x,y), c) | (y,r) <- zip [0..] grid, (x,c) <- zip [0..] r, c /= '.']
  let stepEast  m = M.mapKeys (\k@(x,y) -> let next = (mod (x+1) width, y)  in if M.lookup k m == Just '>' && M.notMember next m then next else k) m
  let stepSouth m = M.mapKeys (\k@(x,y) -> let next = (x, mod (y+1) height) in if M.lookup k m == Just 'v' && M.notMember next m then next else k) m
  let step = stepSouth . stepEast
  let countSteps m = let m' = step m in if m == m' then 1 else 1 + countSteps m'
  print $ countSteps m
