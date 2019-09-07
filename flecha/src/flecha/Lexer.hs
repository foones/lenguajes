
module Lexer(Token(..), tokenize) where

import AST(Id)

data Token = T_DEF
           | T_DEFEQ
           | T_LOWERID Id
           | T_UPPERID Id
           | T_NUMBER Int
           | T_STRING String
           | T_CHAR Char
           | T_SEMICOLON
           | T_LPAREN
           | T_RPAREN
           | T_LAMBDA
           | T_CASE
           | T_IF
           | T_THEN
           | T_ELIF
           | T_ELSE
           | T_LET
           | T_IN
           | T_PIPE
           | T_ARROW
           ---- Builtin operators
           -- Logical
           | T_AND
           | T_OR
           | T_NOT
           -- Arithmetic
           | T_PLUS
           | T_MINUS
           | T_TIMES
           | T_DIV
           | T_MOD
           -- Relational
           | T_EQ
           | T_NE
           | T_LE
           | T_GE
           | T_LT
           | T_GT
  deriving (Show, Eq)

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

isIdent :: Char -> Bool
isIdent c = isDigit c || isUpper c || isLower c || c == '_'

tokenize :: String -> [Token]
tokenize []          = []
-- Whitespace
tokenize (' '  : xs) = tokenize xs
tokenize ('\t' : xs) = tokenize xs
tokenize ('\r' : xs) = tokenize xs
tokenize ('\n' : xs) = tokenize xs
-- Characters
tokenize ('\'' : '\\' : '\\' : '\'' : xs) = T_CHAR '\\' : tokenize xs
tokenize ('\'' : '\\' : '\"' : '\'' : xs) = T_CHAR '\"' : tokenize xs
tokenize ('\'' : '\\' : '\'' : '\'' : xs) = T_CHAR '\'' : tokenize xs
tokenize ('\'' : '\\' : 't' : '\'' : xs)  = T_CHAR '\t' : tokenize xs
tokenize ('\'' : '\\' : 'r' : '\'' : xs)  = T_CHAR '\r' : tokenize xs
tokenize ('\'' : '\\' : 'n' : '\'' : xs)  = T_CHAR '\n' : tokenize xs
tokenize ('\'' : c : '\'' : xs)           = T_CHAR c    : tokenize xs
-- Strings
tokenize ('"' : xs) =
    let (str, xs') = rec xs in
      T_STRING str : tokenize xs'
  where
    rec ('"' : xs) = ([], xs)
    rec ('\\' : '\\' : xs) = add '\\' xs
    rec ('\\' : '\"' : xs) = add '\"' xs
    rec ('\\' : '\'' : xs) = add '\'' xs
    rec ('\\' : 't' : xs)  = add '\t' xs
    rec ('\\' : 'r' : xs)  = add '\r' xs
    rec ('\\' : 'n' : xs)  = add '\n' xs
    rec (c : xs)           = add c xs
    add c xs = let (cs, xs') = rec xs in (c : cs, xs')
-- Numbers
tokenize xs@(c : _) | isDigit c =
  let (n, xs') = span isDigit xs in
    T_NUMBER (read n :: Int) : tokenize xs'
-- Identifiers / keywords
tokenize xs@(c : _) | isIdent c = 
  let (id, xs') = span isIdent xs
      token     =  case id of
                      "def"  -> T_DEF
                      "case" -> T_CASE
                      "if"   -> T_IF
                      "then" -> T_THEN
                      "elif" -> T_ELIF
                      "else" -> T_ELSE
                      "let"  -> T_LET
                      "in"   -> T_IN
                      _ -> if isUpper c
                            then T_UPPERID id
                            else T_LOWERID id
    in token :  tokenize xs'
-- Symbols / operators
tokenize ('-' : '-' : xs) =
  let (_, xs') = span (/= '\n') xs in
    tokenize xs'
tokenize ('=' : '=' : xs) = T_EQ        : tokenize xs
tokenize ('!' : '=' : xs) = T_NE        : tokenize xs
tokenize ('>' : '=' : xs) = T_GE        : tokenize xs
tokenize ('<' : '=' : xs) = T_LE        : tokenize xs
tokenize ('-' : '>' : xs) = T_ARROW     : tokenize xs
tokenize ('&' : '&' : xs) = T_AND       : tokenize xs
tokenize ('|' : '|' : xs) = T_OR        : tokenize xs
tokenize (';' : xs)       = T_SEMICOLON : tokenize xs
tokenize ('=' : xs)       = T_DEFEQ     : tokenize xs
tokenize ('<' : xs)       = T_LT        : tokenize xs
tokenize ('>' : xs)       = T_GT        : tokenize xs
tokenize ('|' : xs)       = T_PIPE      : tokenize xs
tokenize ('\\' : xs)      = T_LAMBDA    : tokenize xs
tokenize ('(' : xs)       = T_LPAREN    : tokenize xs
tokenize (')' : xs)       = T_RPAREN    : tokenize xs
tokenize ('!' : xs)       = T_NOT       : tokenize xs
tokenize ('+' : xs)       = T_PLUS      : tokenize xs
tokenize ('-' : xs)       = T_MINUS     : tokenize xs
tokenize ('*' : xs)       = T_TIMES     : tokenize xs
tokenize ('/' : xs)       = T_DIV       : tokenize xs
tokenize ('%' : xs)       = T_MOD       : tokenize xs

