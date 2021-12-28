import Data.List

fromBits = foldl (\a b -> 2*a+b) 0
mc xs = if sum xs * 2 >= length xs then 1 else 0
lc xs = 1 - mc xs

seek _ f [x] = x
seek i f xs = seek (i+1) f [x | x<-xs, x!!i == f (map (!!i) xs)]

main = do
  txt <- getContents
  let rows = map (map $ read.pure) $ lines txt
  let cols = transpose rows
  let γ = fromBits $ map mc cols
  let ε = fromBits $ map lc cols
  print $ γ*ε
  let oxy = fromBits $ seek 0 mc rows
  let co2 = fromBits $ seek 0 lc rows
  print $ oxy*co2
