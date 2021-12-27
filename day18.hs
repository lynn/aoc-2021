import Aoc
import Parser

import Control.Applicative
import Control.Monad
import Data.Maybe

data Tree = Leaf Int | Branch Tree Tree deriving (Eq, Show)

ch c = pCharIf (==c)

pTree = (Leaf <$> pInt) <|> (Branch <$> (ch '[' *> pTree) <*> (ch ',' *> pTree <* ch ']'))

showTree (Leaf n) = show n
showTree (Branch l r) = "[" ++ showTree l ++ "," ++ showTree r ++ "]"

magnitude (Leaf n) = n
magnitude (Branch l r) = 3 * magnitude l + 2 * magnitude r

-- I stole this way of writing binary tree zippers from Conor McBride.
-- The "gratuitous () where the 'hole' is" is such a nice idea!
-- https://stackoverflow.com/a/25530311/257418
type Context = [Either ((), Tree) (Tree, ())]
type Zipper = (Context, Tree)

-- Oh no, I need to define like, a kajillion zipper operations!!!
goLeft (c, Branch l r) = Just (Left ((), r):c, l)
goLeft _ = Nothing

goRight (c, Branch l r) = Just (Right (l, ()):c, r)
goRight _ = Nothing

goUp (Left  ((), r):c, t) = Just (c, Branch t r)
goUp (Right (l, ()):c, t) = Just (c, Branch l t)
goUp ([], t) = Nothing

sinkLeft (c, Branch l r) = sinkLeft (Left ((), r):c, l)
sinkLeft z@(c, Leaf _) = z
sinkRight z@(c, Branch l r) = sinkRight (Right (l, ()):c, r)
sinkRight z@(c, Leaf _) = z

goNext z@(Left  _:c, t) = (goUp >=> goRight >=> pure . sinkLeft) z
goNext z@(Right _:c, t) = (goUp >=> goNext) z
goNext ([], _) = Nothing

goPrev z@(Left  _:c, t) = (goUp >=> goPrev) z
goPrev z@(Right _:c, t) = (goUp >=> goLeft >=> pure . sinkRight) z
goPrev ([], _) = Nothing

modifyLeaf f (c, Leaf n) = (c, Leaf (f n))

modifyNext f z =
  case goNext z of
    Nothing -> z
    Just z' -> fromJust $ goPrev $ modifyLeaf f z'

modifyPrev f z =
  case goPrev z of
    Nothing -> z
    Just z' -> fromJust $ goNext $ modifyLeaf f z'

zipper t = ([], t)

tree (Left  ((), r):c, t) = tree (c, Branch t r)
tree (Right (l, ()):c, t) = tree (c, Branch l t)
tree ([], t) = t

depth (c, t) = length c

explode (c, Branch (Leaf x) (Leaf y)) =
  modifyPrev (+x) $ modifyNext (+y) $ (c, Leaf 0)

split (c, Leaf n) = (c, Branch (Leaf (div n 2)) (Leaf (div (n+1) 2)))

data Result = Changed Tree | Reduced Tree deriving (Eq, Show)

explodeFirst z@(_, Leaf _)
  | depth z > 4 = Just $ tree $ explode (fromJust $ goUp z)
  | otherwise = goNext z >>= explodeFirst

splitFirst z@(_, Leaf n)
  | n >= 10 = Just $ tree $ split z
  | otherwise = goNext z >>= splitFirst

reduce t =
  let z = sinkLeft $ zipper t in
    case explodeFirst z <|> splitFirst z of
      Just t' -> reduce t'
      Nothing -> t

t1 🐌 t2 = reduce (Branch t1 t2)

main = do
  ls <- getLines
  let trees = runParser pTree <$> ls
  let total = foldl1 (🐌) trees
  putStrLn $ showTree total
  print $ magnitude total
  print $ maximum [magnitude (x 🐌 y) | x <- trees, y <- trees]
