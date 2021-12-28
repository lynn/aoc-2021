import Aoc
import Data.Complex
import Data.List
import qualified Data.Map.Strict as M

readPoint = (\[a,b]->(a,b)) . readInts
increment map k = M.insertWith (+) k 1 map
isOrthogonal ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2
extent ((x1,y1),(x2,y2)) =
  let sx = signum (x2-x1)
      sy = signum (y2-y1)
      n = max (abs (x2-x1)) (abs (y2-y1))
  in [(x1+i*sx, y1+i*sy) | i <- [0..n]]

main = do
  txt <- getContents
  let vents = [(readPoint p, readPoint q) | [p,_,q] <- map words $ lines txt]
  let (orthogonal, diagonal) = partition isOrthogonal vents
  let add vs tally = foldl' increment tally $ extent =<< vs
  let tally = add orthogonal M.empty
  print $ M.size $ M.filter (>=2) tally

  let tally' = add diagonal tally
  print $ M.size $ M.filter (>=2) tally'

