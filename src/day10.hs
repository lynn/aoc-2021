import Aoc
import Data.Either
import Data.List

score '(' = 1
score '[' = 2
score '{' = 3
score '<' = 4

x % [] = Right (foldl (\a c -> 5*a + score c) 0 x)
x % (c:y) | c `elem` "([{<" = (c:x) % y
('(':x) % (')':y) = x % y
('[':x) % (']':y) = x % y
('{':x) % ('}':y) = x % y
('<':x) % ('>':y) = x % y
_ % (')':y) = Left 3
_ % (']':y) = Left 57
_ % ('}':y) = Left 1197
_ % ('>':y) = Left 25137

main = do
  ls <- getLines
  let (errors, completions) = partitionEithers $ map ([]%) ls
  print $ sum errors
  print $ sort completions !! div (length completions) 2

