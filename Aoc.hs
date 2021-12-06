module Aoc where
import Data.Char

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

readInts :: String -> [Integer]
readInts = map read . words . map noJunk
  where noJunk x = if x == '-' || isDigit x then x else ' '

getInts :: IO [Integer]
getInts = readInts <$> getLine

paragraphs :: [String] -> [[String]]
paragraphs xs = filter (>[]) $ go [] xs
  where go p ("":xs) = reverse p : go [] xs
        go p (x:xs) = go (x:p) xs
        go p [] = [reverse p]

getParagraphs :: IO [[String]]
getParagraphs = paragraphs . lines <$> getContents

