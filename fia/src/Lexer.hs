module Lexer(tokenize) where

import qualified Data.Map as M

import FailState(FailState, evalFS, failFS)
import Token(Token(..), isDigit, isIdent)

data LexerState = LexerState {}

type M = FailState LexerState

tokenize :: String -> Either String [Token]
tokenize source = evalFS (tokenizeM source) initialState
  where initialState = LexerState

keywords :: M.Map String Token
keywords = M.fromList [
             ("theorem", TTheorem)
           , ("fun", TFun)
           , ("prop", TProp)
           , ("eval", TEval)
           , ("proof", TProof)
           , ("end", TEnd)
           --
           , ("forall", TForall)
           , ("exists", TExists)
           --
           , ("let", TLet)
           , ("suppose", TSuppose)
           , ("indeed", TIndeed)
           , ("induction", TInduction)
           , ("contradiction", TContradiction)
           , ("show", TShow)
           , ("claim", TClaim)
           , ("then", TThen)
           , ("also", TAlso)
           , ("have", THave)
           , ("assume", TAssume)
           , ("cases", TCases)
           , ("case", TCase)
           , ("take", TTake)
           , ("consider", TConsider)
           , ("st", TSt)
           , ("by", TBy)
           , ("S", TSucc)
           ]

symbols :: [(String, Token)]
symbols = [
             (":=", TDefEq)
           , ("->", TFormImp)
           , ("|", TFormOr)
           , ("&", TFormAnd)
           , (":", TColon)
           , (",", TComma)
           , ("#", THash)
           , ("(", TLParen)
           , (")", TRParen)
           , ("[", TLBrack)
           , ("]", TRBrack)
           , ("+", TAdd)
           , ("*", TMul)
           , ("=", TEq)
           ]

tokenizeM :: String -> M [Token]
tokenizeM "" = return []
tokenizeM (' ' : cs)  = tokenizeM cs
tokenizeM ('\t' : cs) = tokenizeM cs
tokenizeM ('\r' : cs) = tokenizeM cs
tokenizeM ('\n' : cs) = tokenizeM cs
tokenizeM cs@('-' : '-' : _) =
  let (_, cs') = span (/= '\n') cs
   in do tokenizeM cs'
tokenizeM ('{' : '-' : cs) = rec 0 cs
  where
    rec n ('{' : '-' : cs) = rec (n + 1) cs
    rec n ('-' : '}' : cs)
      | n > 0     = rec (n - 1) cs
      | otherwise = tokenizeM cs
    rec n (_ : cs) = rec n cs
    rec _ [] = failFS "Premature end of multiline comment."
tokenizeM cs@(c : _) | isDigit c =
  let (num, cs') = span isDigit cs
   in do toks <- tokenizeM cs'
         return (TNatConst (read num) : toks)
tokenizeM ('?' : cs) =
  let (holeName, cs') = span isIdent cs
   in do toks <- tokenizeM cs'
         return (THole holeName : toks)
tokenizeM cs@(c : _) | isIdent c =
  let (id, cs') = span isIdent cs
   in do toks <- tokenizeM cs'
         case M.lookup id keywords of
           Just tok -> return (tok : toks)
           Nothing  -> return (TId id : toks)
tokenizeM cs =
  let matching = [(tok, drop (length symbol) cs)
                 | (symbol, tok) <- symbols, take (length symbol) cs == symbol]
   in case matching of
        ((tok, cs') : _) -> do toks <- tokenizeM cs'
                               return (tok : toks)
        [] -> failFS ("Unrecognized input: " ++ show (take 10 cs) ++ "...")

