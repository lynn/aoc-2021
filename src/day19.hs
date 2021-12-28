import Aoc
import Control.Monad
import Data.List
import qualified Data.Set as S
import Debug.Trace

data Thing = Beacon | Scanner deriving (Eq, Ord, Show)
readBeacon = (\[x,y,z] -> (Beacon,x,y,z)) . readInts
beacons = S.filter (\(t,_,_,_) -> t == Beacon)
scanners = S.filter (\(t,_,_,_) -> t == Scanner)

a ∩ b = S.intersection a b
a ∪ b = S.union a b
deleteAt i xs = l ++ r where (l,_:r) = splitAt i xs
distance (_,x1,y1,z1) (_,x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

evenPerms = [\(t,x,y,z)->(t,x,y,z),\(t,x,y,z)->(t,y,z,x),\(t,x,y,z)->(t,z,x,y)]
oddPerms  = [\(t,x,y,z)->(t,y,x,z),\(t,x,y,z)->(t,z,y,x),\(t,x,y,z)->(t,x,z,y)]

rotations set = [S.map (applySigns sgn . p) set | sgn <- replicateM 3 [1,-1],
    p <- if even (count (-1==) sgn) then evenPerms else oddPerms]
    where applySigns [sx,sy,sz] (t,x,y,z) = (t,sx*x,sy*y,sz*z)

-- Rotate and translate s2 in all ways that have 12+ matches with s1.
-- If we find one, return the translated s2.
match s1 s2 =
  [tr2 | rot2 <- rotations s2,
         (Beacon,x1,y1,z1) <- S.toList s1,
         (Beacon,x2,y2,z2) <- S.toList rot2,
         let translate (t,x,y,z) = (t,x+x1-x2,y+y1-y2,z+z1-z2),
         let tr2 = S.map translate rot2,
         S.size (beacons s1 ∩ beacons tr2) >= 12]

-- Match two sets and merge them into the same frameof reference, until we find no more matches.
matchAll sets =
  let r = [0..traceShowId (length sets) - 1] in
  case [(i,j,m) | i<-r, j<-r, i<j, m <- match (sets!!i) (sets!!j)] of
    (i,j,m):_ -> matchAll $ (sets!!i ∪ m) : deleteAt i (deleteAt j sets)
    [] -> sets

main = do
  ps <- getParagraphs
  let sets = [S.fromList $ (Scanner,0,0,0) : map readBeacon ls | _:ls <- ps]
  let [combined] = matchAll sets
  putStr "*:  "
  print $ S.size $ beacons combined
  let ss = S.toList (scanners combined)
  putStr "**: "
  print $ maximum [distance s1 s2 | s1 <- ss, s2 <- ss]
