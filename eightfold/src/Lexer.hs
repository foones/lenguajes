module Lexer where

import Data.Char(isUpper, isLower, isDigit)

type Id = String

data Token = TokenColon
           | TokenFun
           | TokenId Id
           | TokenLParen
           | TokenRParen
           | TokenDot
           | TokenComma
           | TokenProof
           | TokenAsk
           | TokenInvalid
           deriving Show

isBlank :: Char -> Bool 
isBlank x = x `elem` " \n\t\r"

isSymbol :: Char -> Bool 
isSymbol x = x `elem` "_+-*/~"

--
isIdent :: String -> Bool
isIdent xs = isShortIdent xs || isNumIdent xs || isLongIdent xs

isShortIdent :: String -> Bool
isShortIdent xs = length xs > 0 && isLower (head xs)

isNumIdent :: String -> Bool
isNumIdent xs = length xs > 0 && isDigit (head xs)

isLongIdent :: String -> Bool
isLongIdent xs = length xs > 0 && isUpper (head xs)

--

tokenize :: String -> [Token]
tokenize (x:xs)
  | isBlank x = tokenize xs
tokenize (x:xs)
  | isLower x || isSymbol x = TokenId y : tokenize ys
     where y  = x : takeWhile p xs
           ys = dropWhile p xs
           p x = isDigit x
tokenize (x:xs)
  | isDigit x = TokenId y : tokenize ys
     where y  = x : takeWhile p xs
           ys = dropWhile p xs
           p x = isDigit x
tokenize (x:xs)
  | isUpper x = TokenId y : tokenize ys
     where y  = x : takeWhile p xs
           ys = dropWhile p xs
           p x = isLower x || isDigit x || x == '_'
tokenize ('"':xs) = TokenId y : tokenize ys
  where y  = "\"" ++ takeWhile p xs ++ "\""
        ys = tail $ dropWhile p xs
        p x = x /= '"'
tokenize ('#':xs)           = tokenize ys
  where ys = dropWhile (/= '\n') xs
tokenize ('=':xs)           = TokenProof : tokenize xs
tokenize (':':xs)           = TokenColon : tokenize xs
tokenize ('.':xs)           = TokenDot : tokenize xs
tokenize (',':xs)           = TokenComma : tokenize xs
tokenize ('(':xs)           = TokenLParen : tokenize xs
tokenize (')':xs)           = TokenRParen : tokenize xs
tokenize ('>':xs)           = TokenFun : tokenize xs
tokenize ('?':xs)           = TokenAsk : tokenize xs
tokenize []                 = []
tokenize _                  = [TokenInvalid]

