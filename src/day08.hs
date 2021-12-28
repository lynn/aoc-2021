import Aoc
import Data.List

x ⊆ y = sort x `isSubsequenceOf` sort y

solve line =
  let
    (before, _:after) = break (=="|") $ words line
    b n = [x | x <- before, length x == n]
    -- Bin them by length:
    [[one], [seven], [four], b5, b6, [eight]] = b <$> [2..7]
    -- Tell apart 2,3,5:
    [three] = [x | x <- b5, one ⊆ x]
    [five] = [x | x <- b5, four ⊆ (x++one)]
    [two] = b5 \\ [three, five]
    -- Tell apart 0,6,9:
    [six] = [x | x <- b6, not $ seven ⊆ x]
    [nine] = [x | x <- b6, four ⊆ x]
    [zero] = b6 \\ [six, nine]
    -- Translate the digits after the |:
    digits = sort <$> [zero,one,two,three,four,five,six,seven,eight,nine]
    decoded = [lookup (sort d) $ zip digits ['0'..'9'] | d <- after]
    Just n = read <$> sequence decoded
  in n

main = do
  ls <- getLines
  print $ sum [1 | l <- ls, w <- tail.snd.break(=="|") $ words l, length w `elem` [2,3,4,7]]
  print $ sum [solve l | l <- ls]
