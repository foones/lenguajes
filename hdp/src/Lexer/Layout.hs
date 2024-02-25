module Lexer.Layout(layout) where

import FailState(FailState, getFS, putFS, failFS, evalFS)

import Position(Position(..), unknownPosition)
import Error(Error(..), ErrorType(..))
import Lexer.Token(Token(..), TokenType(..), isLBrace, isRBrace)

layout :: [Token] -> Either Error [Token]
layout tokens = evalFS (lay (prepare tokens) []) initialPosition
  where
    initialPosition
      | null tokens = unknownPosition
      | otherwise   = tokenStartPos (head tokens)

---- Implementation of the layout algorithm,
---- following Haskell '98 standard (sec. 9.3).

data LToken = T Token
            | A Integer -- <n>
            | B Integer -- {n}
            | C         -- force closing implicit brace

line :: Token -> Integer
line t = positionLine (tokenStartPos t)

column :: Token -> Integer
column t = positionColumn (tokenStartPos t)

isBeginModuleKeyword :: Token -> Bool
isBeginModuleKeyword t =
  case tokenType t of
    T_Module -> True
    T_LBrace -> True
    _        -> False

isLayoutKeyword :: Token -> Bool
isLayoutKeyword t =
  case tokenType t of
    T_Where  -> True
    T_Of     -> True
    T_Fresh  -> True
    T_Let    -> True
    T_Mutual -> True
    _        -> False

isCloseLayoutKeyword :: Token -> Bool
isCloseLayoutKeyword t =
  case tokenType t of
    T_In -> True
    _    -> False

startsWithLBrace :: [Token] -> Bool
startsWithLBrace []      = False
startsWithLBrace (t : _) = isLBrace t

-- Prepare input for layout algorithm

prepare :: [Token] -> [LToken]
prepare []         = []
prepare ts@(t : _) =
    if isBeginModuleKeyword t
     then rec False False 0 ts
     else B (column t) : rec True (isRBrace t) 0 ts
  where
    rec :: Bool     -- Has {n} been inserted right before?
        -> Bool     -- Is the previous token a closing brace?
        -> Integer  -- Line of previous token 
        -> [Token]
        -> [LToken]
    rec _ _  _ [] = []
    rec b br l (t : ts)
      | isCloseLayoutKeyword t && line t == l && not br =
        -- If the current token is a closing layout keyword such as "in",
        -- on the same line as the previous token, and there is not a
        -- closing brace ("}") before, forcibly insert a closing brace.
        C : rec b True l (t : ts)
    rec b _ l (t : ts)
      | isLayoutKeyword t && not (startsWithLBrace ts) =
         continue b l t ++ T t : B n : rec True (isRBrace t) (line t) ts
      where n = if null ts
                 then 0
                 else column (head ts)
    rec b _ l (t : ts) =
         continue b l t ++ T t : rec False (isRBrace t) (line t) ts

    continue False l t | line t > l = [A (column t)]
    continue _     _ _              = []

-- Layout algorithm

type LayoutState = Position
type M = FailState LayoutState

putPos :: Token -> M ()
putPos t = putFS (tokenStartPos t)

newToken :: TokenType -> M Token
newToken x = do
  pos <- getFS
  return $ Token pos pos x

lay :: [LToken] -> [Integer] -> M [Token]
lay (A n : ts) (m : ms)
  | m == n                = do ls <- lay ts (m : ms)
                               tok <- newToken T_Semicolon
                               return (tok : ls)
  | n < m                 = do ls <- lay (A n : ts) ms
                               tok <- newToken T_RBrace 
                               return (tok : ls)
lay (A _ : ts) ms         = lay ts ms
lay (B n : ts) (m : ms)
  | n > m                 = do ls <- lay ts (n : m : ms)
                               tok <- newToken T_LBrace 
                               return (tok : ls)
lay (B n : ts) []
  | n > 0                 = do ls <- lay ts [n]
                               tok <- newToken T_LBrace 
                               return (tok : ls)
lay (B n : ts) ms         = do ls <- lay (A n : ts) ms
                               tok1 <- newToken T_LBrace
                               tok2 <- newToken T_RBrace
                               return (tok1 : tok2 : ls)
lay (T t : ts) (0 : ms)
  | isRBrace t            = do putPos t
                               ls <- lay ts ms
                               return (t : ls)
lay (T t : ts) ms 
  | isRBrace t            = do putPos t
                               failFS (Error LexerErrorLayout (tokenStartPos t)
                                         "Explicit close brace matches \
                                         \implicit open brace.")
lay (T t : ts) ms
  | isLBrace t            = do putPos t
                               ls <- lay ts (0 : ms)
                               tok <- newToken T_LBrace 
                               return (tok : ls)
lay (T t : ts) ms         = do putPos t
                               ls <- lay ts ms
                               return (t : ls)
lay []         []         = return []
lay []         (m : ms)
  | m > 0                 = do ls <- lay [] ms
                               tok <- newToken T_RBrace 
                               return (tok : ls)
lay (C : ts) (m : ms)     = do tok <- newToken T_RBrace 
                               ls <- lay ts ms
                               return (tok : ls)
lay _          _          = do pos <- getFS
                               failFS $ Error LexerErrorLayout
                                              pos "Layout error."

