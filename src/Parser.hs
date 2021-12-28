{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

many1 p = (:) <$> p <*> many p

newtype Parser a = Parser { parse :: String -> [(a, String)] } deriving (Functor)

instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])
  (<*>)  = ap

instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\s -> case parse p s of [] -> parse q s; res -> res)

instance Monad Parser where
  p >>= f = Parser (\s -> do (a, s') <- parse p s
                             parse (f a) s')
pChar :: Parser Char
pChar = Parser $ \case ""     -> []
                       (c:cs) -> [(c,cs)]

pCharIf :: (Char -> Bool) -> Parser Char
pCharIf f = Parser $ \case (c:cs) | f c -> [(c,cs)]; _ -> []

pDigit :: Parser Int
pDigit = digitToInt <$> pCharIf isDigit

pInt :: Parser Int
pInt = foldl1 (\a x -> 10*a+x) <$> many1 pDigit

untilEof :: Parser a -> Parser [a]
untilEof p = Parser $ \case "" -> [([],"")]
                            s  -> parse ((:) <$> p <*> untilEof p) s

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(a, [])]   -> a
    [(_, rest)] -> error "Parser did not consume entire stream."
    _           -> error "Couldn't parse."
