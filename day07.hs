import Aoc

tri n = n * (n+1) `div` 2

main = do
  xs <- getInts
  let n = maximum xs
  print $ minimum [sum [      abs (x-h) | x <- xs] | h <- [0..n]]
  print $ minimum [sum [tri $ abs (x-h) | x <- xs] | h <- [0..n]]

