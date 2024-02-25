
module Lexer.Categories(
         isDigit, isInteger, isWhitespace,
         isSoftPunctuation, softPunctuationType,
         isPunctuation, punctuationType,
         isKeyword, keywordType, isIdent,
         isRenaming, renaming
       ) where

import qualified Data.Map as M
import Data.Char(isPrint)

import Lexer.Token(TokenType(..))

-- Identifiers may not contain punctuation
punctuation :: M.Map Char TokenType
punctuation = M.fromList [
  ('.', T_Dot),
  ('(', T_LParen),
  (')', T_RParen),
  ('{', T_LBrace),
  ('}', T_RBrace),
  (';', T_Semicolon),
  (',', T_Id ","),
  (':', T_Id ":")
 ]

-- Identifiers may contain soft punctuation
softPunctuation :: M.Map Char TokenType
softPunctuation = M.fromList [
  ('!', T_Id "!")
 ]

keywords :: M.Map String TokenType
keywords = M.fromList [
  ("as", T_As),
  ("case", T_Case),
  ("class", T_Class),
  ("data", T_Data),
  ("eval", T_Eval),
  ("fresh", T_Fresh),
  ("=", T_Eq),
  ("import", T_Import),
  ("in", T_In),
  ("infix", T_Infix),
  ("infixl", T_Infixl),
  ("infixr", T_Infixr),
  ("instance", T_Instance),
  ("\\", T_Lambda),
  ("λ", T_Lambda),
  ("let", T_Let),
  ("module", T_Module),
  ("mutual", T_Mutual),
  ("type", T_Type),
  ("of", T_Of),
  ("where", T_Where)
 ]

renamings :: M.Map String String
renamings = M.fromList [
  ("<+>", "⊕"),
  ("<*>", "⊗"),
  ("->", "→"),
  ("_->_", "_→_"),
  ("forall", "∀"),
  ("d<*>", "δ⊗"),
  ("d&1", "δ&₁"),
  ("d&2", "δ&₂"),
  ("d<+>", "δ⊕"),
  ("d!", "δ!")
 ]

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isInteger :: String -> Bool
isInteger s = not (null s) && all isDigit s

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\r', '\n']

isSoftPunctuation :: Char -> Bool
isSoftPunctuation c = M.member c softPunctuation

softPunctuationType :: Char -> TokenType
softPunctuationType c = M.findWithDefault undefined c softPunctuation

isPunctuation :: Char -> Bool
isPunctuation c = M.member c punctuation

punctuationType :: Char -> TokenType
punctuationType c = M.findWithDefault undefined c punctuation

isKeyword :: String -> Bool
isKeyword s = M.member s keywords

keywordType :: String -> TokenType
keywordType s = M.findWithDefault undefined s keywords

isIdent :: Char -> Bool
isIdent c = isPrint c && not (isWhitespace c) && not (isPunctuation c)

isRenaming :: String -> Bool
isRenaming s = M.member s renamings

renaming :: String -> String
renaming s = M.findWithDefault undefined s renamings

