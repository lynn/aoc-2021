import Aoc
import Control.Monad
import Data.Maybe

data State = On | Off deriving (Eq, Show)

readCuboid = (\[xl,xh,yl,yh,zl,zh] -> (xl,xh+1,yl,yh+1,zl,zh+1)) . readInts
readInstruction ["on", c] = (On, readCuboid c)
readInstruction ["off", c] = (Off, readCuboid c)

nonEmpty (xl,xh,yl,yh,zl,zh) = xl < xh && yl < yh && zl < zh
volume (xl,xh,yl,yh,zl,zh) = (xh-xl)*(yh-yl)*(zh-zl)
intersects (xl1,xh1,yl1,yh1,zl1,zh1) (xl2,xh2,yl2,yh2,zl2,zh2) =
  let xl = max xl1 xl2
      xh = min xh1 xh2
      yl = max yl1 yl2
      yh = min yh1 yh2
      zl = max zl1 zl2
      zh = min zh1 zh2
  in nonEmpty (xl,xh,yl,yh,zl,zh)

-- I didn't want to figure out this math so I peeked at robinhouston's solution.
-- Thanks! https://gist.github.com/robinhouston/2c9dc527d95357c5aa19e33e5368fb01
diff c@(cxl,cxh,cyl,cyh,czl,czh) e@(exl,exh,eyl,eyh,ezl,ezh) =
  let dxl = max cxl exl
      dxh = min cxh exh
      dyl = max cyl eyl
      dyh = min cyh eyh
      dzl = max czl ezl
      dzh = min czh ezh
      parts = [ (cxl,cxh,cyl,cyh,czl,dzl)
              , (cxl,cxh,cyl,cyh,dzh,czh)
              , (dxl,cxh,cyl,dyl,dzl,dzh)
              , (dxh,cxh,dyl,cyh,dzl,dzh)
              , (cxl,dxh,dyh,cyh,dzl,dzh)
              , (cxl,dxl,cyl,dyh,dzl,dzh) ]
  in if intersects c e then filter nonEmpty parts else [c]

(Off, c) % (vol, cs) = (vol, c:cs)
(On, c)  % (vol, cs) = (vol + sum (map volume $ foldM diff c cs), c:cs)

main = do
  instructions <- map (readInstruction . words) <$> getLines
  let easy = intersects (-50,51,-50,51,-50,51) . snd
  print $ fst $ foldr (%) (0,[]) $ filter easy instructions
  print $ fst $ foldr (%) (0,[]) $ instructions
