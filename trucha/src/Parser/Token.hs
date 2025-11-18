
module Parser.Token(Token(..), TokenType(..)) where

import qualified Syntax.Position as Pos
import Syntax.Name(QName, HoleName)

data Token = Token {
               startPos  :: Pos.Position
             , tokenType :: TokenType
             }
  deriving Show

data TokenType =
    TId QName
  | THole HoleName
  | TInt Integer
  | TEof
  -- Delimiters
  | TSemicolon
  | TLBrace
  | TRBrace
  | TLParen
  | TRParen
  -- Dot
  | TDot
  -- Keywords
  | TData
  | TWhere
  | TLet
  | TIn
  | TCase
  | TOf
  | TModule
  | TImport
  | TInfix
  | TInfixL
  | TInfixR
  | TEq
  | TColon
  | TUnderscore
  | TLambda
  | TArrow
  | TForall
  | TExists
  deriving (Eq, Show)

