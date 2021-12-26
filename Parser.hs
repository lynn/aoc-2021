{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

import Control.Monad

newtype Parser a = Parser { parse :: String -> [(a, String)] } deriving (Functor)

instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])
  (<*>)  = ap

instance Monad Parser where
  p >>= f = Parser (\s -> do (a, s') <- parse p s
                             parse (f a) s')
pChar :: Parser Char
pChar = Parser $ \case ""     -> []
                       (c:cs) -> [(c,cs)]

untilEof :: Parser a -> Parser [a]
untilEof p = Parser $ \case "" -> [([],"")]
                            s  -> parse ((:) <$> p <*> untilEof p) s

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(a, [])]   -> a
    [(_, rest)] -> error "Parser did not consume entire stream."
    _           -> error "Couldn't parse."
