import Aoc

step [n0,n1,n2,n3,n4,n5,n6,n7,n8] = [n1,n2,n3,n4,n5,n6,n7+n0,n8,n0]

main = do
  fish <- getInts
  let state = [count (==i) fish | i <- [0..8]]
  let history = iterate step state
  print $ sum (history !! 80)
  print $ sum (history !! 256)
