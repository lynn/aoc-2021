import Aoc
import qualified Data.Map as M
import Data.List

main = do
  [[seed], ruleText] <- getParagraphs
  let pairs = zip seed (tail seed)
  let rules = M.fromList [((x,y),z) | [[x,y],_,[z]] <- words <$> ruleText]
  let s0 = M.fromListWith (+) [(p,1) | p <- pairs]
  let f ((x,y),n) = [((x,p),n), ((p,y),n)] where p = rules M.! (x,y)
  let step s = M.fromListWith (+) $ f =<< M.assocs s
  let counts s = M.fromListWith (+) $ (last seed,1) : [(x,n) | ((x,_),n) <- M.toList s]
  let extent a = maximum a - minimum a
  let solve n = extent $ map snd $ M.toList $ counts $ iterate step s0 !! n
  print $ solve 10
  print $ solve 40
