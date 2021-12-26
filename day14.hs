{-# LANGUAGE TupleSections #-}
import Aoc
import qualified Data.Map as M
import Data.List

extent a = maximum a - minimum a

main = do
  [[seed], ruleText] <- getParagraphs
  let rules = M.fromList [((x,y),z) | [[x,y],_,[z]] <- words <$> ruleText]

  -- Track counts for each pair.
  let seedPairs = zip seed (tail seed)
  let s0 = M.fromListWith (+) $ (,1) <$> seedPairs

  -- An insertion rule like XY -> C means that
  -- `n` XY pairs give rise to `n` XC pairs and `n` CY pairs.
  let f ((x,y),n) = [((x,c),n), ((c,y),n)] where c = rules M.! (x,y)
  let step s = M.fromListWith (+) $ f =<< M.toList s

  -- To get counts for each element, count the left halves of all our pairs,
  -- then throw in the last element of our original seed.
  let lefts s = [(x,n) | ((x,_),n) <- M.toList s]
  let counts s = M.fromListWith (+) $ (last seed,1) : lefts s
  let solve n = extent $ map snd $ M.toList $ counts $ iterate step s0 !! n
  print $ solve 10
  print $ solve 40