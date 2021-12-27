import Aoc
import Control.Monad
import Data.List
import Data.Function.Memoize

play p1 p2 s1 s2 rolls (x:y:z:die)
  | s2 >= 1000 = s1 * rolls
  | s1 >= 1000 = error "impossible"
  | otherwise = let p1' = mod(p1+x+y+z-1)10+1 in play p2 p1' s2 (s1+p1') (rolls+3) die

distribution = [(k, n) | k <- [1+1+1..3+3+3], let n = count ((==k).sum) $ replicateM 3 [1,2,3]]

sum2 = foldl' (\(a,b) (x,y) -> (a+x,b+y)) (0,0)
n *~ (a,b) = (n*a,n*b)

dirac p1 p2 s1 s2 p1turn
  | s1 >= 21 = (1,0)
  | s2 >= 21 = (0,1)
  | p1turn    = sum2 [let p1' = mod(p1+k-1)10+1 in n *~ dirac' p1' p2 (s1+p1') s2 False | (k,n) <- distribution]
  | otherwise = sum2 [let p2' = mod(p2+k-1)10+1 in n *~ dirac' p1 p2' s1 (s2+p2') True  | (k,n) <- distribution]

dirac' = memoize5 dirac

main = do
  [1,p1] <- getInts
  [2,p2] <- getInts
  print $ play p1 p2 0 0 0 (cycle [1..100])
  print $ uncurry max $ dirac p1 p2 0 0 True
