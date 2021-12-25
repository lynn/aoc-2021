import Aoc
import qualified Data.Map as M

zeros = M.fromList [((x,y),0) | y <- [0..9], x <- [0..9]]
nc (x,y) = [(x+dx, y+dy) | dx<-[-1..1], dy<-[-1..1],
            0<=x+dx, x+dx<=9, 0<=y+dy, y+dy<=9, (dx,dy)/=(0,0)]
next (x,y) = M.fromList [(p,1) | p <- nc (x,y)]

flashes acc m =
  case M.partition (>9) m of
    (e, m') | M.null e -> (acc, M.union m' zeros)
    (f, m') -> flashes (acc + M.size f) (M.intersection mNext m')
      where mNext = M.unionsWith (+) (m' : [next p | p <- M.keys f])

step = flashes 0 . M.map (+1)

totalFlashes 0 _ = 0
totalFlashes s m = let (f, m') = step m in f + totalFlashes (s-1) m'

sync m = if all (==0) (M.elems m) then 0 else sync (snd (step m)) + 1

main = do
  ls <- getLines
  let m = M.fromList [((x,y),read[n]) | (y,l) <- zip [0..] ls, (x,n) <- zip [0..] l]
  print $ totalFlashes 100 m
  print $ sync m
