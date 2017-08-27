
module Lexer(Token(..), tokenize) where

data Token =
    T_ID String
  | T_NUM Integer
  | T_LPAREN
  | T_RPAREN
  | T_COMMA
  | T_LBRACK
  | T_RBRACK
  | T_LBRACE
  | T_RBRACE
  | T_ASSIGN
  | T_COLON
  | T_HASH
  | T_LE
  | T_GE
  | T_LT
  | T_GT
  | T_EQ
  | T_NE
  | T_PLUS
  | T_MINUS
  | T_TIMES
  | T_BOOL
  | T_INT
  | T_VEC
  | T_TRUE
  | T_FALSE
  | T_AND
  | T_ELSE
  | T_FUN
  | T_IF
  | T_NOT
  | T_OR
  | T_RETURN
  | T_WHILE
  deriving Show

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = ('a' <= c && c <= 'z')
         || ('A' <= c && c <= 'Z')
         || c == '_'

isIdent :: Char -> Bool
isIdent c = isAlpha c || isDigit c

keywords :: [(String, Token)]
keywords = [
  ("Bool", T_BOOL),
  ("Int", T_INT),
  ("Vec", T_VEC),
  ("True", T_TRUE),
  ("False", T_FALSE),
  ("and", T_AND),
  ("else", T_ELSE),
  ("fun", T_FUN),
  ("if", T_IF),
  ("not", T_NOT),
  ("or", T_OR),
  ("return", T_RETURN),
  ("while", T_WHILE)
 ]

tokenize :: String -> [Token]
tokenize "" = []
tokenize (' ' : s)         = tokenize s
tokenize ('\t' : s)        = tokenize s
tokenize ('\r' : s)        = tokenize s
tokenize ('\n' : s)        = tokenize s
tokenize ('(' : s)         = T_LPAREN : tokenize s
tokenize (')' : s)         = T_RPAREN : tokenize s
tokenize (',' : s)         = T_COMMA : tokenize s
tokenize ('[' : s)         = T_LBRACK : tokenize s
tokenize (']' : s)         = T_RBRACK : tokenize s
tokenize ('{' : s)         = T_LBRACE : tokenize s
tokenize ('}' : s)         = T_RBRACE : tokenize s
tokenize (':' : '=' : s)   = T_ASSIGN : tokenize s
tokenize (':' : s)         = T_COLON : tokenize s
tokenize ('#' : s)         = T_HASH : tokenize s
tokenize ('<' : '=' : s)   = T_LE : tokenize s
tokenize ('>' : '=' : s)   = T_GE : tokenize s
tokenize ('<' : s)         = T_LT : tokenize s
tokenize ('>' : s)         = T_GT : tokenize s
tokenize ('=' : '=' : s)   = T_EQ : tokenize s
tokenize ('!' : '=' : s)   = T_NE : tokenize s
tokenize ('+' : s)         = T_PLUS : tokenize s
tokenize ('-' : s)         = T_MINUS : tokenize s
tokenize ('*' : s)         = T_TIMES : tokenize s
tokenize s@('/' : '/' : _) =
  let (_, s') = span (not . (== '\n')) s in
    tokenize s'
tokenize s@(x : _) | isDigit x =
  let (n, s') = span isDigit s in
    T_NUM (read n :: Integer) : tokenize s'
tokenize s@(x : _) | isIdent x =
  let (id, s') = span isIdent s in
    case lookup id keywords of
      Just k  -> k : tokenize s'
      Nothing -> T_ID id : tokenize s'

