
module Parser(tokenize, parseProgram) where

import qualified Data.Map as M

import Syntax(Id, Expr(..), Declaration(..))

data Token = TEof
           | TId Id
           | THole Id
           | TType
           | TLParen
           | TRParen
           | TLambda
           | TColon
           | TArrow
           | TAxiom
           | TDef
           | TEqual
           | TCheck
           | TRewrite
  deriving (Eq, Show)

isTId :: Token -> Bool
isTId (TId _) = True
isTId _       = False

startsExpression :: Token -> Bool
startsExpression (TId _)   = True
startsExpression (THole _) = True
startsExpression TType     = True
startsExpression TLambda   = True
startsExpression TLParen   = True
startsExpression _         = False

isTerminator :: Token -> Bool
isTerminator tok = not (startsExpression tok)

isIdent :: Char -> Bool
isIdent c = ('0' <= c && c <= '9')
         || ('A' <= c && c <= 'Z')
         || ('a' <= c && c <= 'z')
         || c `elem` "!#$%&*+-/:?@\\_|<>=."

keywords :: M.Map Id Token
keywords = M.fromList [
             ("axiom", TAxiom)
           , ("Type", TType)
           , ("\\", TLambda)
           , ("->", TArrow)
           , (":", TColon)
           , ("def", TDef)
           , ("check", TCheck)
           , ("=", TEqual)
           , ("rewrite", TRewrite)
           ]

tokenize :: String -> [Token]
tokenize ""         = []
tokenize (' ' : s)  = tokenize s
tokenize ('\t' : s) = tokenize s
tokenize ('\r' : s) = tokenize s
tokenize ('\n' : s) = tokenize s
tokenize ('-' : '-' : s) = rec s
  where
    rec ('\n' : s) = tokenize s
    rec (_ : s)    = rec s
tokenize s@('{' : '-' : _) = rec 0 s
  where
    rec n ('{' : '-' : s) = rec (n + 1) s
    rec n ('-' : '}' : s) = if n == 1
                             then tokenize s
                             else rec (n - 1) s
    rec n (_ : s) = rec n s
tokenize ('(' : s)  = TLParen : tokenize s
tokenize (')' : s)  = TRParen : tokenize s
tokenize s@(c : _)
  | isIdent c = let (ident, s') = span isIdent s
                 in if c == '?'
                     then THole (tail ident) : tokenize s'
                     else case M.lookup ident keywords of
                            Just keyword -> keyword : tokenize s'
                            Nothing      -> TId ident : tokenize s'
tokenize _ = error "Entrada mal formada."

----

data Parser a = PP ([Token] -> (a, [Token]))

getParserState :: Parser [Token]
getParserState = PP $ \ toks -> (toks, toks)

putParserState :: [Token] -> Parser ()
putParserState toks = PP $ \ _ -> ((), toks)

evalParser :: Parser a -> [Token] -> a
evalParser (PP ma) toks = fst (ma toks)

instance Functor Parser where
  fmap f (PP ma) = PP $ \ toks ->
    let (a, toks') = ma toks
     in (f a, toks')

instance Applicative Parser where
  pure a = PP $ \ toks -> (a, toks)
  PP mf <*> PP ma = PP $ \ toks ->
    let (f, toks') = mf toks
        (a, toks'') = ma toks'
     in (f a, toks'')

instance Monad Parser where
  return = pure
  PP ma >>= f = PP $ \ toks ->
    let (a, toks') = ma toks
        PP mb = f a
     in mb toks'

peekToken :: Parser Token
peekToken = PP $ \ toks -> 
  case toks of
    (tok : _) -> (tok, toks)
    [] -> (TEof, toks)

nextToken :: Parser ()
nextToken = PP $ \ toks -> 
  case toks of
    (_ : toks') -> ((), toks')
    [] -> ((), [])

matchId :: Parser Id
matchId = PP $ \ toks ->
  case toks of
    (TId x : toks) -> (x, toks)
    _ -> error ("Se esperaba un identificador.\n" ++
                "Se encontró: " ++ show (head (toks ++ [TEof])))

matchToken :: Token -> Parser ()
matchToken tok = PP $ \ toks ->
  case toks of
    (tok' : toks) | tok == tok' -> ((), toks)
    _ -> error ("Se esperaba: " ++ show tok ++ ".\n" ++
                "Se encontró: " ++ show (head (toks ++ [TEof])))

parseProgram :: [Token] -> [Declaration]
parseProgram = evalParser parseDeclarations

parseExpr :: Parser Expr
parseExpr = parseArrows

parseArrows :: Parser Expr
parseArrows = do
  t1 <- parseApplication
  tok <- peekToken
  case tok of
    TArrow -> do
      matchToken TArrow
      t2 <- parseArrows
      return (Pi "{}" t1 t2)
    _ -> return t1

parseApplication :: Parser Expr
parseApplication = do
    func <- parseAtom
    rec func
  where
    rec :: Expr -> Parser Expr
    rec app = do
      tok <- peekToken
      if isTerminator tok
       then return app
       else do
         arg <- parseAtom
         rec (App app arg)

foundParamList :: Parser Bool
foundParamList = do
  toks0 <- getParserState
  case toks0 of
    (TLParen : toks1) ->
      let (idList, toks2) = span isTId toks1 in
        case toks2 of
          (TColon : toks3) -> return True
          _ -> return False
    _ -> return False

parseIdSequence :: Parser [Id]
parseIdSequence = do
  tok <- peekToken
  case tok of
    TId x -> do
      nextToken
      xs <- parseIdSequence
      return (x : xs)
    _ -> return []

parseParamList :: Parser [(Id, Expr)]
parseParamList = do
  matchToken TLParen
  ids <- parseIdSequence
  matchToken TColon
  typ <- parseExpr
  matchToken TRParen
  return (map (\ id -> (id, typ)) ids)

parseTypeParamLists :: Parser [(Id, Expr)]
parseTypeParamLists = do
  tok <- peekToken
  case tok of
    TArrow -> return []
    TColon -> return []
    _ -> do
      xs1 <- parseParamList
      xs2 <- parseTypeParamLists
      return (xs1 ++ xs2)

parseLambdaParamLists :: Parser [(Id, Maybe Expr)]
parseLambdaParamLists = do
  tok <- peekToken
  case tok of
    TArrow -> return []
    TId x -> do
      nextToken
      xs2 <- parseLambdaParamLists
      return ((x, Nothing) : xs2)
    _ -> do
      xs1 <- parseParamList
      xs2 <- parseLambdaParamLists
      return (map (\ (x, t) -> (x, Just t)) xs1 ++ xs2)

parseType :: Parser Expr
parseType = do
  b <- foundParamList
  if b
   then do
     pList <- parseTypeParamLists
     matchToken TArrow
     body <- parseExpr
     return (foldr (uncurry Pi) body pList)
   else
     parseExpr

parseLambda :: Parser Expr
parseLambda = do
  matchToken TLambda
  pList <- parseLambdaParamLists
  matchToken TArrow
  body <- parseExpr
  return (foldr (uncurry Lam) body pList)

parseAtom :: Parser Expr
parseAtom = do
  tok <- peekToken
  case tok of
    TId x -> do
      nextToken
      return $ Var x
    THole name -> do
      nextToken
      return $ Hole name
    TType -> do
      nextToken
      return $ Type
    TLambda -> do
      parseLambda
    TLParen -> do
      b <- foundParamList
      if b
       then parseType
       else do
         nextToken
         expr <- parseExpr
         matchToken TRParen
         return expr
    _ -> error "Se esperaba una expresión."

parsePattern :: Parser Expr
parsePattern = parsePatternApplication

parsePatternApplication :: Parser Expr
parsePatternApplication = do
    func <- parsePatternAtom
    rec func
  where
    rec :: Expr -> Parser Expr
    rec app = do
      tok <- peekToken
      if isTerminator tok
       then return app
       else do
         arg <- parseAtom
         rec (App app arg)

parsePatternAtom :: Parser Expr
parsePatternAtom = do
  tok <- peekToken
  case tok of
    TId x -> do
      nextToken
      return $ Var x
    TLParen -> do
      nextToken
      expr <- parsePattern
      matchToken TRParen
      return expr
    _ -> error "Se esperaba un patrón."

parseDeclaration :: Parser Declaration
parseDeclaration = do
  tok <- peekToken
  case tok of
    TAxiom -> do
      matchToken TAxiom
      name <- matchId
      matchToken TColon
      typ <- parseExpr
      return $ DeclAxiom name typ
    TDef -> do
      matchToken TDef
      name <- matchId
      tok <- peekToken
      params <- case tok of
                  TLParen -> do
                    params <- parseTypeParamLists
                    return params
                  _ -> return []
      matchToken TColon
      typ <- parseExpr
      matchToken TEqual
      body <- parseExpr
      let typ' = foldr (uncurry Pi) typ params
      let body' = foldr (\ (x, typ) -> Lam x (Just typ)) body params
      return $ DeclDef name typ' body'
    TCheck -> do
      matchToken TCheck
      expr <- parseExpr
      return $ DeclCheck expr
    TRewrite -> do 
      matchToken TRewrite
      tok <- peekToken
      params <- case tok of
                  TLParen -> do
                    params <- parseTypeParamLists
                    matchToken TColon
                    return params
                  _ -> return []
      lhs <- parsePattern
      matchToken TEqual
      rhs <- parseExpr
      return $ DeclRewrite params lhs rhs
    _ -> error "Se esperaba una declaración."

parseDeclarations :: Parser [Declaration]
parseDeclarations = do
  tok <- peekToken
  case tok of
    TEof -> return []
    _ -> do
      decl  <- parseDeclaration
      decls <- parseDeclarations
      return (decl : decls)

