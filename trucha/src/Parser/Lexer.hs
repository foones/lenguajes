module Parser.Lexer(Token(..), TokenType(..), tokenize, readQName) where

import qualified Data.Map as M

import Utils(thenFail, splitBy)
import qualified Syntax.Position as Pos
import FailState(ErrMsg, FailState, evalFS, getFS, modifyFS, failFS)
import Syntax.Name(
         ModuleName, makeModuleName, BareName, makeBareName,
         HoleName(..), BareNamePart(..), QName, makeQName, rootModuleName
       )
import Parser.Token(Token(..), TokenType(..))

----

keywords :: M.Map String TokenType
keywords = M.fromList [
             ("data", TData)
           , ("where", TWhere)
           , ("let", TLet)
           , ("in", TIn)
           , ("case", TCase)
           , ("of", TOf)
           , ("module", TModule)
           , ("import", TImport)
           , ("infix", TInfix)
           , ("infixl", TInfixL)
           , ("infixr", TInfixR)
           , ("=", TEq)
           , (":", TColon)
           , ("_", TUnderscore)
           , ("λ", TLambda)
           , ("∀", TForall)
           , ("∃", TExists)
           ]

isKeyword :: String -> Bool
isKeyword str = str `M.member` keywords

keywordType :: String -> TokenType
keywordType str = M.findWithDefault undefined str keywords

delimiters :: M.Map Char TokenType
delimiters = M.fromList [
               (';', TSemicolon)
             , ('{', TLBrace)
             , ('}', TRBrace)
             , ('(', TLParen)
             , (')', TRParen)
             ]

isDelimiter :: Char -> Bool
isDelimiter c = c `M.member` delimiters

delimiterType :: Char -> TokenType
delimiterType c = M.findWithDefault undefined c delimiters

----

tokenize :: String -> String -> Either ErrMsg [Token]
tokenize filename source =
    evalFS initialState (layout =<< tokenizeM source)
  where
    initialState = LexerState {
      position = Pos.start filename
    , expectToplevelWhere = False
    }

data LexerState =
  LexerState {
    position :: Pos.Position
  -- The expectToplevelWhere flag is used by the layout algorithm
  -- to allow the contents of a toplevel 'where'
  -- to *not* be indented.
  -- A toplevel 'where' is the 'where' accompanying "module ... where"
  -- if the 'module' keyword is at column 1.
  , expectToplevelWhere :: Bool
  }

type M = FailState LexerState

tokenizeM :: String -> M [Token]
tokenizeM ""       = do
  pos <- getPos
  return [Token pos TEof]
tokenizeM cs@('-' : '-' : _) = do
  let (comment, cs') = span (/= '\n') cs
  readStringM comment
  tokenizeM cs'
tokenizeM cs@('{' : '-' : _) = do
    pos <- getPos
    rec pos 0 cs
  where
    rec :: Pos.Position -> Integer -> String -> M [Token]
    rec pos n [] = failM pos "Unclosed multiline comment."
    rec pos n ('{' : '-' : cs) = do
      readStringM "{-"
      rec pos (n + 1) cs
    rec pos n ('-' : '}' : cs) = do
      readStringM "-}"
      if n == 1
       then tokenizeM cs
       else rec pos (n - 1) cs
    rec pos n (c : cs) = do
      readCharM c
      rec pos n cs
tokenizeM (c : cs) | isWhitespace c  = do
  readCharM c
  tokenizeM cs
tokenizeM ('.' : cs) = do
  pos <- getPos
  readCharM '.'
  toks <- tokenizeM cs
  return (Token pos TDot : toks)
tokenizeM (c : cs) | isDelimiter c = do
  pos <- getPos
  readCharM c
  toks <- tokenizeM cs
  return (Token pos (delimiterType c) : toks)
tokenizeM cs@(c : _) | isDigit c = do
  let (num, cs') = span isDigit cs
  pos <- getPos
  readStringM num
  toks <- tokenizeM cs'
  return (Token pos (TInt (read num :: Integer)) : toks)
tokenizeM cs@(c : _) | isIdent c = do
  let (name, cs') = span isIdent cs
  pos <- getPos
  readStringM name
  tokenType <-
    case name of
      '?' : hname -> return $ THole (HoleName hname)
      _ | isKeyword name -> return $ keywordType name
      _ -> do qName <- case readQName name of
                Left msg    -> failM pos msg
                Right qName -> return qName
              return $ TId qName
  toks <- tokenizeM cs'
  return (Token pos tokenType : toks)
tokenizeM (c : _) = do
  pos <- getPos
  failM pos ("Invalid character found: " ++ show c)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' '
              || c == '\t'
              || c == '\r'
              || c == '\n'

isIdent :: Char -> Bool
isIdent c = not (isWhitespace c) && not (isDelimiter c)

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

readCharM :: Char -> M ()
readCharM chr = do
  modifyFS (\ state -> state { position = Pos.afterChar chr (position state) })

readStringM :: String -> M ()
readStringM str = do
  modifyFS (\ state -> state { position = Pos.after str (position state) })

getPos :: M Pos.Position
getPos = position <$> getFS

readQName :: String -> Either String QName
readQName "_" = Right $ makeQName rootModuleName (makeBareName ["_"])
readQName str = do
  let parts = splitBy (== '.') str
  any null parts 
    `thenFail` "Malformed qualified name."
  any isKeyword parts
    `thenFail` "Malformed qualified name: name parts cannot be keywords."
  let moduleName  = makeModuleName (init parts)
  let bareNameStr = last parts
  bareName <- readBareName bareNameStr
  return $ makeQName moduleName bareName

readBareName :: String -> Either String BareName
readBareName str = do
    bareNameStrParts <- rec str 
    any (\ str -> str /= "_" && isKeyword str) bareNameStrParts
      `thenFail` "Malformed barename: name parts cannot be keywords."
    return $ makeBareName bareNameStrParts
  where
    rec :: String -> Either String [String]
    rec "" = return []
    rec ('_' : s) = do
      recs <- rec s
      case recs of
        ("_" : _) -> Left "Malformed barename: consecutive underscores."
        rs        -> return ("_" : rs)
    rec (c : s) = do
      recs <- rec s
      case recs of
        []         -> return ([c] : [])
        ("_" : rs) -> return ([c] : "_" : rs)
        (r : rs)   -> return ((c : r) : rs)

failM :: Pos.Position -> ErrMsg -> M a
failM pos msg = do
  failFS $ unlines [
             "Near: " ++ show pos
           , "Lexer error"
           , msg
           ]

---- Layout algorithm

-- RR represents a "real" brace (written by the user)
-- VV represents a "virtual" brace at a given column (introduced by layout)
data Opener = RR
            | VV Integer

layout :: [Token] -> M [Token]
layout tokens = do
    res <- closePendingWithoutSeparator [VV 1] tokens
    return (Token (posOf tokens) TLBrace : res)
  where
    closePending :: [Opener] -> [Token] -> M [Token]
    closePending fullStack@(VV n : stack) (tok : toks)
      | column tok == n && not (isEof tok) = do
          res <- open fullStack (tok : toks)
          return (makeAt tok TSemicolon : res)
    closePending stack toks = closePendingWithoutSeparator stack toks

    closePendingWithoutSeparator :: [Opener] -> [Token] -> M [Token]
    closePendingWithoutSeparator fullStack@(VV n : stack) (tok : toks)
      | column tok < n || isEof tok = do
          res <- closePending stack (tok : toks)
          return (makeAt tok TRBrace : res)
      | isRBrace tok =
          failM (startPos tok) "Invalid layout: closing virtual brace with '}'."
      | isClosingKeyword tok && not (null stack) = do
          res <- closePending stack toks
          return (makeAt tok TRBrace : tok : res)
      | otherwise = open fullStack (tok : toks)
    closePendingWithoutSeparator (RR : stack) (tok : toks)
      | isRBrace tok = do
          res <- closePending stack toks
          return (tok : res)
    closePendingWithoutSeparator fullStack input = open fullStack input

    open :: [Opener] -> [Token] -> M [Token]
    open stack (tok : toks)
      | isLBrace tok = do
          res <- closePending (RR : stack) toks
          return (tok : res)
    open stack (tok : toks@(nextTok : _))
      | isLayout tok && isLBrace nextTok = do 
          res <- closePending stack toks
          return (tok : res)
    open stack (tok : toks@(nextTok : _))
      | isLayout tok = do
        bExpectToplevelWhere <- expectToplevelWhere <$> getFS
        let isToplevelWhere = isWhere tok && bExpectToplevelWhere
        if isToplevelWhere
         then modifyFS (\ state -> state { expectToplevelWhere = False })
         else return ()
        if not (isEof nextTok) &&
           (canStartColumn stack (column nextTok) || isToplevelWhere)
         then do
           res <- closePendingWithoutSeparator (VV (column nextTok) : stack) toks
           return (tok : makeAt tok TLBrace : res)
         else do
          res <- closePending stack toks
          return (tok : makeAt tok TLBrace : makeAt tok TRBrace : res)
    open stack (tok : [])
      | isEof tok = return []
    open stack (tok : _)
      | isEof tok = error "(Should not find end-of-file)"
    open stack (tok : toks)
      | isModule tok && column tok == 1 = do
        modifyFS (\ state -> state { expectToplevelWhere = True })
        res <- closePending stack toks
        return (tok : res)
    open stack (tok : toks) = do
      res <- closePending stack toks
      return (tok : res)
    open _ [] = error "(Impossible)"

    --
    canStartColumn :: [Opener] -> Integer -> Bool
    canStartColumn []            _   = True
    canStartColumn (RR : _)      _   = True
    canStartColumn (VV col0 : _) col = col0 < col
    --
    posOf :: [Token] -> Pos.Position
    posOf []        = Pos.unknown
    posOf (tok : _) = startPos tok
    --
    column :: Token -> Integer
    column = Pos.column . startPos
    --
    makeAt :: Token -> TokenType -> Token
    makeAt tok tokType = Token (startPos tok) tokType
    --
    tokenTypeIs :: TokenType -> Token -> Bool
    tokenTypeIs tokType tok = tokenType tok == tokType
    --
    isModule :: Token -> Bool
    isModule = tokenTypeIs TModule
    --
    isWhere :: Token -> Bool
    isWhere = tokenTypeIs TWhere
    --
    isLBrace :: Token -> Bool
    isLBrace = tokenTypeIs TLBrace
    --
    isRBrace :: Token -> Bool
    isRBrace = tokenTypeIs TRBrace
    --
    isEof :: Token -> Bool
    isEof = tokenTypeIs TEof
    --
    isLayout :: Token -> Bool
    isLayout tok = tokenTypeIs TWhere tok
                || tokenTypeIs TOf tok
                || tokenTypeIs TLet tok
    --
    isClosingKeyword :: Token -> Bool
    isClosingKeyword = tokenTypeIs TIn

