import Aoc
import Data.List
import qualified Data.Set as S

fromBin = foldl' (\a b->2*a+b) 0 
s ? (x,y) = if S.member (x,y) s then 1 else 0
fringe s = S.fromList [(x+dx,y+dy) | (x,y) <- S.toList s, dy<-[-1..1], dx<-[-1..1]]

step algo s =
  let live (x,y) = '#' == algo !! fromBin [s?(x+dx,y+dy) | dy<-[-1..1],dx<-[-1..1]]
  in S.filter live (fringe s)

invert = map (\x -> if x == '#' then '.' else '#')

main = do
  [[algo], grid] <- getParagraphs
  let s0 = S.fromList [(x,y) | (y,r) <- zip [0..] grid, (x,'#')<- zip [0..] r]
  -- I can't explain why this works. merry christmas
  let twoSteps = step (reverse algo) . step (invert algo)
  print $ S.size $ iterate twoSteps s0 !! 1
  print $ S.size $ iterate twoSteps s0 !! 25
