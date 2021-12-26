import Aoc
import Parser (pChar, parse, untilEof, runParser)

import Data.Char
import Data.List
import Control.Monad

type Version = Int
data Packet = Lit Version Int | Op Version Int [Packet] deriving (Eq, Show)

hexToBin c = replicateM 4 "01" !! digitToInt c
binToInt = foldl' (\acc x -> 2 * acc + read [x]) 0
field n = binToInt <$> replicateM n pChar

pLiteral acc = do
  b <- field 1
  n <- field 4
  let n' = 16 * acc + n
  case b of
    0 -> pure n'
    1 -> pLiteral n'

pPacket = do
  v <- field 3
  t <- field 3
  case t of
    4 -> Lit v <$> pLiteral 0
    op -> do
      i <- field 1
      case i of
        0 -> do
          bits <- field 15
          packetBits <- replicateM bits pChar
          pure $ Op v op $ runParser (untilEof pPacket) packetBits
        1 -> do
          num <- field 11
          Op v op <$> replicateM num pPacket

sumVersions (Lit v _) = v
sumVersions (Op v _ ps) = v + sum (sumVersions <$> ps)

compute (Lit _ i) = i
compute (Op v op ps) =
  let two g [x,y] = if g x y then 1 else 0
      f = case op of
            0 -> sum
            1 -> product
            2 -> minimum
            3 -> maximum
            5 -> two (>)
            6 -> two (<)
            7 -> two (==)
  in f (compute <$> ps)

main = do
  g <- getLine
  let bits = g >>= hexToBin
  let packet = runParser (pPacket <* untilEof pChar) bits
  print $ sumVersions packet
  print $ compute packet
