incs xs = sum [1 | (a,b) <- zip xs (tail xs), a<b]
win3 xs = zipWith3 (\a b c->a+b+c) xs (tail xs) (drop 2 xs)

main = do
  text <- getContents
  let xs = map read $ lines text
  print $ incs xs
  print $ incs (win3 xs)

