(h, v) % ["forward", x] = (h+read x, v)
(h, v) % ["down", x] = (h, v+read x)
(h, v) % ["up", x] = (h, v-read x)

(h, v, a) %% ["down", x] = (h, v, a+read x)
(h, v, a) %% ["up", x] = (h, v, a-read x)
(h, v, a) %% ["forward", x] = (h+read x, v+a*read x, a)

main = do
  txt <- getContents
  let commands = map words $ lines txt
  let (h,v) = foldl (%) (0,0) commands in print (h*v)
  let (h,v,_) = foldl (%%) (0,0,0) commands in print (h*v)

