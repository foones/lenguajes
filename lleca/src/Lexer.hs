module Lexer(Token(..), TokenizerConfig(..), tokenize, isIdent) where

import Data.List(sortBy, isPrefixOf)
import Data.Ord(comparing)

data Token = T_Id String
           | T_Num Integer
           | T_String String
           | T_Literal String
  deriving Show

data TokenizerConfig = TokenizerConfig {
                         tokenizerKeywords :: [String],
                         tokenizerPunctuation :: [String]
                       }

isAlpha :: Char -> Bool
isAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') 

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isIdent :: Char -> Bool
isIdent c = isAlpha c || isDigit c || c == '_'

tokenize :: TokenizerConfig -> String -> [Token] 
tokenize (TokenizerConfig keywords symbols) =
    tokenize' (TokenizerConfig keywords symbols')
  where
    symbols' = reverse (sortBy (comparing length) symbols)

tokenize' :: TokenizerConfig -> String -> [Token] 
tokenize' conf []              = []
tokenize' conf (' ' : s)       = tokenize' conf s
tokenize' conf ('\t' : s)      = tokenize' conf s
tokenize' conf ('\r' : s)      = tokenize' conf s
tokenize' conf ('\n' : s)      = tokenize' conf s
tokenize' conf ('/' : '*' : s) = tokenize' conf (rec s)
  where
    rec []             = error "Unclosed comment"
    rec ('*' : '/': s) = s
    rec (_ : s)        = rec s
tokenize' conf s | anyMatch    =
    T_Literal chosenSymbol : tokenize' conf s'
  where
    candidateSymbols =
      [x | x <- tokenizerPunctuation conf, isPrefixOf x s]
    anyMatch         = not (null candidateSymbols)
    chosenSymbol     = head candidateSymbols
    s'               = drop (length chosenSymbol) s
tokenize' conf s@(c : _)
  | isDigit c = let (n, s') = span isDigit s
                 in T_Num (read n) : tokenize' conf s'
  | isIdent c = let (id, s') = span isIdent s
                 in if id `elem` tokenizerKeywords conf
                     then T_Literal id : tokenize' conf s'
                     else T_Id id : tokenize' conf s'
  | c == '"'  = let (str, s') = readString (tail s)
                 in T_String str : tokenize' conf s'
tokenize' _ xs = error ("Cannot tokenize: " ++ xs)

readString :: String -> (String, String)
readString ('"' : s)         = ("", s)
readString ('\\' : '\\' : s) = let (str, s') = readString s
                                in ('\\' : str, s')
readString ('\\' : '"' : s)  = let (str, s') = readString s
                                in ('"' : str, s')
readString (c : s)           = let (str, s') = readString s
                                in (c : str, s')

