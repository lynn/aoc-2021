import Aoc
import Data.List

-- Does `ns` win on this board?
win ns b = any (all (`elem` ns)) $ b ++ transpose b

-- Draw from `next` and yield n*sum(unmarked) for each board that's won.
play bs _ [] = []
play bs drawn (n:next) =
  let (won, lost) = partition (win (n:drawn)) bs
  in [n * sum (concat b \\ (n:drawn)) | b <- won] ++ play lost (n:drawn) next

main = do
  draws <- getInts
  boards <- map (map readInts) <$> getParagraphs
  let scores = play boards [] draws
  print (head scores)
  print (last scores)
