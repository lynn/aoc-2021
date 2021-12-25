import Aoc
import qualified Data.Map as M
import Data.List

fwd = M.findWithDefault

paths k graph src tgt visited
  | src == tgt = [[tgt]]
  | otherwise = [src:p |
      n <- fwd [] src graph,
      let lim = if n == "start" || n == "end" then 1 else k,
      n < "a" || fwd 0 n visited < lim,
      let k' = if n >= "a" && fwd 0 n visited == 1 then 1 else k,
      p <- paths k' graph n tgt (M.insertWith (+) src 1 visited)]

main = do
  ls <- getLines
  let edges = [(p,q) | (p,_:q) <- map (break (=='-')) ls]
  let bothWays = edges ++ [(q,p) | (p,q) <- edges]
  let graph = M.unionsWith (++) [M.singleton p [q] | (p,q) <- bothWays]
  print $ length $ paths 1 graph "start" "end" M.empty
  print $ length $ paths 2 graph "start" "end" M.empty
