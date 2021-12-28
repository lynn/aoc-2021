{-# LANGUAGE DeriveGeneric #-}

import Aoc
import Data.Graph.AStar (aStar)
import Data.Hashable
import Debug.Trace
import qualified Data.HashSet as H
import GHC.Generics (Generic)
import qualified Data.Array as A

visArray = A.listArray ((-1,0),(3,13)) "#############\n#...........#\n###.#.#.#.###\n  #.#.#.#.#  \n  #########  \n"

coords (Start x y) = (y,x)
coords (Home x y) = (y,x)
coords (Hallway x) = (0,x)
sprite 3 = 'A'
sprite 5 = 'B'
sprite 7 = 'C'
sprite 9 = 'D'
visualize state = A.elems $ visArray A.// traceShowId [(coords loc, sprite g) | Amphipod g loc <- state]

data Location = Start Int Int -- x∈{3,5,7,9} y∈{1,2}
              | Hallway Int -- x∈{1,2,4,6,8,10,11}
              | Home Int Int
              deriving (Eq, Ord, Show, Generic)
data Amphipod = Amphipod Int Location -- goal x∈{3,5,7,9} and location
              deriving (Eq, Ord, Show, Generic)

type State = [Amphipod]
instance Hashable Location
instance Hashable Amphipod

s0easy = [ Amphipod 9 (Start 3 1)
         , Amphipod 9 (Start 3 2)
         , Amphipod 7 (Start 5 1)
         , Amphipod 7 (Start 5 2)
         , Amphipod 3 (Start 7 1)
         , Amphipod 5 (Start 7 2)
         , Amphipod 5 (Start 9 1)
         , Amphipod 3 (Start 9 2)
         ]

s0hard = [ Amphipod 9 (Start 3 1)
         , Amphipod 9 (Start 3 2)
         , Amphipod 9 (Start 3 3)
         , Amphipod 9 (Start 3 4)
         , Amphipod 7 (Start 5 1)
         , Amphipod 7 (Start 5 2)
         , Amphipod 5 (Start 5 3)
         , Amphipod 7 (Start 5 4)
         , Amphipod 3 (Start 7 1)
         , Amphipod 5 (Start 7 2)
         , Amphipod 3 (Start 7 3)
         , Amphipod 5 (Start 7 4)
         , Amphipod 5 (Start 9 1)
         , Amphipod 3 (Start 9 2)
         , Amphipod 7 (Start 9 3)
         , Amphipod 3 (Start 9 4)
         ]

between x1 x2 x = x >= min x1 x2 && x <= max x1 x2

moves s = do
  i <- [0..length s - 1]
  let Amphipod goal location = s!!i
  let others = take i s ++ drop (i+1) s

  new <- do
    case location of
      Home _ _ -> []
      Hallway h -> do
        let blocksHome (Amphipod _ l) =
              case l of
                Start x' y' -> x' == goal
                Hallway h' -> between h goal h'
                Home _ _ -> False
        let sameHome (Amphipod g l) =
              case l of
                Home _ _ -> g == goal
                _ -> False
        let maxY = div (length s) 4
        if any blocksHome others then [] else [Home goal (maxY - count sameHome others)]
      Start x y -> do
        target <- [1,2,4,6,8,10,11]
        let blocksHallway (Amphipod _ l) =
              case l of
                Start x' y' -> x' == x && y' < y
                Hallway h' -> between x target h'
                Home _ _ -> False
        if any blocksHallway others then [] else [Hallway target]

  [take i s ++ [Amphipod goal new] ++ drop (i+1) s]

dist (Start x y) (Hallway h) = y + abs (x-h)
dist (Hallway h) (Home x y) = y + abs (x-h)

heuristic (Amphipod g (Home _ _)) = 0
heuristic (Amphipod g (Hallway x)) = abs (x-g)
heuristic (Amphipod g (Start x y)) = abs (x-g) + y + 1

cost a1@(Amphipod g l1) a2@(Amphipod _ l2)
  | a1 == a2 = 0
  | otherwise = dist l1 l2 * 10 ^ (div g 2 - 1)

stateCost s1 s2 = sum $ zipWith cost s1 s2

isHome (Amphipod _ (Home _ _)) = True
isHome _ = False

solve s0 = do
  let Just path = (s0 :) <$> aStar (H.fromList . moves) stateCost (sum . map heuristic) (all isHome) s0
  -- putStr . visualize $ path !! 0
  -- sequence [print (stateCost x y) >> putStr (visualize y) | (x,y) <- zip path (tail path)]
  putStrLn "Total:"
  let totalCost = sum [stateCost x y | (x,y) <- zip path (tail path)]
  print totalCost

main = do
  solve s0easy
  solve s0hard
