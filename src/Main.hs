module Main where

-- https://arxiv.org/pdf/chao-dyn/9506003.pdf

-- import Text.Parsec
import Text.ParserCombinators.Parsec
import System.Environment

main :: IO ()
main = do 
         (code:_) <- getArgs
         putStrLn (run code)

run :: String -> String
run code = case parse parseExpr "" code of
  Left err -> "Error: " ++ show err
  Right value -> show value

{-
This is a weird lisp, and Chaitin's description doesn't always make sense.
So I've tried to make this compatible, but sensical.

This describes the s-language, the m-language is separate

Syntax:
  Whitepace is ignored
  Atoms are single characters or the empty list ()
    Here Chaitin allows any ASCII, but that's silly
    We'll allow 42-122 which is almost all the good ones
  A list is a ( and then zero or more sexps and then )
  A sexp is an atom or a list
  ' is quote: ('(abc)) == (abc)
  & is lambda: (& (x y z) (f x y z)) == f
  : is let, see Chaitin95
  
Predefined:
  1,0 are true and false
  +,- are head and tail
  * is join/dot/pair: (* 'a '(bc)) == (abc)
  . is atom?
  / is ternary if: (/ 0 'a 'b)==b etc.
-}

-- see https://hackage.haskell.org/package/lispparser-0.3.1/docs/src/Text-ParserCombinators-Parsec-Lisp.html for strategy

data Symbol = Null | Symbol Char
        deriving (Show, Eq, Ord)

data Expr = Atom Symbol
          | List [Expr]
          | Quote Expr
          deriving (Show)

parseNull :: Parser Expr
parseNull = do
  _ <- try $ string "()"
  return (Atom Null)

parseSymbol :: Parser Expr
parseSymbol = do
  atom <- oneOf ['*'..'z'] -- includes all numbers, letters, and good symbols
  return $ Atom $ Symbol atom

parseAtom :: Parser Expr
parseAtom = parseNull <|> parseSymbol

parseList :: Parser Expr
parseList = do
  char '('
  exps <- many1 parseExpr
  char ')'
  return $ List exps

parseSexp = parseAtom <|> parseList

parseQuote = do
  char '\''
  s <- parseSexp
  return $ Quote s
  
parseExpr = parseQuote <|> parseSexp
  
