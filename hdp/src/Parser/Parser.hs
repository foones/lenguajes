module Parser.Parser(parse, parseAndGetNamespace) where

import qualified Data.Set as S
import Data.List(isPrefixOf)

import Error(Error(..), ErrorType(..), ErrorMessage)
import Position(Position(..), unknownPosition)
import FailState(FailState, getFS, modifyFS, putFS, evalFS, runFS, failFS)
import Syntax.Name(
         QName(..), readName, qualify, moduleNameFromQName,
         isWellFormedOperatorName, unqualifiedName, splitParts,
         allNameParts,
         modulePRIM, moduleMain, arrowSymbol, colonSymbol,
         primitiveMain,
         primitiveType,
         primitiveForall, primitiveColon, primitiveBang,
         primitiveTensor, primitiveWith, primitiveLinSum,
         primitiveArrow, primitiveUnit, primitiveUnderscore,
         primitiveTensorElim,
         primitiveWithIntro, primitiveWithElim1, primitiveWithElim2,
         primitiveLinSumIntro1, primitiveLinSumIntro2, primitiveLinSumElim,
         primitiveBangElim
       )
import Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr,
         exprIsVariable, exprHeadVariable
       )
import Lexer.Token(Token(..), TokenType(..))

import ModuleSystem.Namespace(Namespace, makeNamespace)
import ModuleSystem.Module(
         Module,
           emptyModule, addSubmodule, exportAllNamesFromModule, exportNames,
           declareName
       )
import ModuleSystem.Context(
         Context,
           emptyContext, contextCurrentModuleName,
           resolveName, importAllNamesFromModule, importNames,
           declareModuleAlias
       )
import ModuleSystem.PrecedenceTable(
         PrecedenceTable(..), Associativity(..),
         PrecedenceLevel, Precedence, precedenceTableLevels,
         emptyPrecedenceTable, declareOperator, isOperator, isOperatorPart,
         operatorAssociativity
       )

parse :: [Token] -> Either Error Program
parse tokens = evalFS parseM (initialState tokens)

parseAndGetNamespace :: [Token] -> Either Error (Program, Namespace)
parseAndGetNamespace tokens =
  case runFS parseM (initialState tokens) of
    Left err -> Left err
    Right (program, state) ->
      Right (program, makeNamespace (stateRootModule state)
                                    (stateNameContext state)
                                    (statePrecedenceTable state))

initialState :: [Token] -> ParserState
initialState tokens =
  ParserState {
    stateInput           = tokens,
    statePosition        = tokensPosition tokens unknownPosition,
    stateRootModule      = emptyModule,
    stateNameContext     = error "Empty name context.",
    statePrecedenceTable = emptyPrecedenceTable,
    stateScopeLevel      = 0 
  }

---- Some constants

defaultAssociativity :: Associativity
defaultAssociativity = NonAssoc

defaultPrecedence :: Precedence
defaultPrecedence = 200

---- Parser monad

data ParserState = ParserState {
                     stateInput           :: [Token],
                     statePosition        :: Position,
                     stateRootModule      :: Module,
                     stateNameContext     :: Context,
                     statePrecedenceTable :: PrecedenceTable,
                     stateScopeLevel      :: Integer --   0 : toplevel scope
                                                     -- > 0 : nested scope
                   }

type M = FailState ParserState

tokensPosition :: [Token] -> Position -> Position
tokensPosition tokens elsePos =
  if null tokens
   then elsePos
   else tokenStartPos (head tokens)

peek :: M Token
peek = do
  state <- getFS
  pos   <- currentPosition
  if null (stateInput state)
   then return $ Token pos pos T_EOF
   else return $ head (stateInput state) 

peekType :: M TokenType
peekType = do
  tok <- peek
  return $ tokenType tok

peekIsAny :: [TokenType] -> M Bool
peekIsAny ts = do
  t <- peekType
  return (t `elem` ts)

peekIs :: TokenType  -> M Bool
peekIs t = peekIsAny [t]

getToken :: M Token
getToken = do
  state <- getFS
  pos   <- currentPosition
  if null (stateInput state)
   then return $ Token pos pos T_EOF
   else let (tok : toks) = stateInput state in do
          putFS (state {
                   stateInput    = toks,
                   statePosition = tokensPosition toks pos
                 })
          return tok

match :: TokenType -> M ()
match t = do
  t' <- peekType
  if t == t'
   then do getToken
           return ()
   else failM ParseErrorExpectedToken
              ("Expected: " ++ show t ++ ".\n" ++
               "Got     : " ++ show t' ++ ".")

currentPosition :: M Position
currentPosition = do
  state <- getFS
  return $ statePosition state

failM :: ErrorType -> String -> M a
failM errorType msg = do
  pos <- currentPosition
  failFS (Error errorType pos msg)

getRootModule :: M Module
getRootModule = do
  state <- getFS
  return $ stateRootModule state

getNameContext :: M Context
getNameContext = do
  state <- getFS
  return $ stateNameContext state

getPrecedenceTable :: M PrecedenceTable
getPrecedenceTable = do
  state <- getFS
  return $ statePrecedenceTable state

getCurrentModuleName :: M QName
getCurrentModuleName = do
  nameContext <- getNameContext
  return $ contextCurrentModuleName nameContext

modifyRootModule :: (Module -> Either ErrorMessage Module) -> M ()
modifyRootModule f = do
  state <- getFS
  case f (stateRootModule state) of
    Left errmsg ->
      failM ModuleSystemError errmsg
    Right stateRootModule' ->
      putFS (state { stateRootModule = stateRootModule' })

modifyNameContext :: (Context -> Either ErrorMessage Context) -> M ()
modifyNameContext f = do
  state <- getFS
  case f (stateNameContext state) of
    Left errmsg ->
      failM ModuleSystemError errmsg
    Right stateNameContext' ->
      putFS (state { stateNameContext = stateNameContext' })

modifyPrecedenceTable ::
  (PrecedenceTable -> Either ErrorMessage PrecedenceTable) -> M ()
modifyPrecedenceTable f = do
  state <- getFS
  case f (statePrecedenceTable state) of
    Left errmsg ->
      failM ModuleSystemError errmsg
    Right precedenceTable' ->
      putFS (state { statePrecedenceTable = precedenceTable' })

enterModule :: QName -> M ()
enterModule moduleName = do
  state <- getFS
  case addSubmodule moduleName (stateRootModule state) of
    Left  errmsg ->
      failM ModuleSystemError errmsg
    Right stateRootModule' ->
      putFS (state {
        stateRootModule  = stateRootModule',
        stateNameContext = emptyContext moduleName,
        stateScopeLevel  = 0
      })

exportAllNamesFromModuleM :: QName -> M ()
exportAllNamesFromModuleM moduleName =
  modifyRootModule (exportAllNamesFromModule moduleName)

exportNamesM :: QName -> [String] -> M ()
exportNamesM moduleName exportedNames =
  modifyRootModule (exportNames moduleName exportedNames)

declareQNameM :: QName -> M ()
declareQNameM qname = do
  isTop <- isToplevelScopeM
  if isTop
   then do moduleName <- getCurrentModuleName
           let uname = unqualifiedName qname in
             if qname == qualify moduleName uname ||
                qname == primitiveMain
              then do -- Declare name in current module
                      modifyRootModule (declareName qname)
                      -- If it is an undeclared operator, declare it
                      declareIfOperatorM qname
              else failM ModuleSystemError
                         ("Declaration of name \"" ++ uname ++ "\"" ++
                          " shadows \"" ++ show qname ++ "\".")
   else -- Names not in the toplevel are not declared
        return ()

importAllNamesFromModuleM :: QName -> M ()
importAllNamesFromModuleM moduleName = do
  rootModule <- getRootModule
  modifyNameContext (importAllNamesFromModule moduleName rootModule)

importNamesM :: QName -> [(String, String)] -> M ()
importNamesM moduleName renamings = do
  rootModule <- getRootModule
  modifyNameContext (importNames moduleName renamings rootModule)

declareModuleAliasM :: QName -> String -> M ()
declareModuleAliasM moduleName alias = do
  rootModule <- getRootModule
  modifyNameContext (declareModuleAlias moduleName alias rootModule)

declareOperatorM :: Associativity -> Precedence -> QName -> M ()
declareOperatorM assoc precedence qname = do
  -- Note: the order of the two following lines cannot be changed,
  -- as declareQNameM declares a name with default associativity/precedence
  -- if it is an operator.
  modifyPrecedenceTable (declareOperator assoc precedence qname)
  declareQNameM qname

-- If the name is a currently undeclared operator,
-- declare it with default associativity and precedence
declareIfOperatorM :: QName -> M ()
declareIfOperatorM qname = do
  table <- getPrecedenceTable
  if isWellFormedOperatorName uname && not (isOperator qname table)
   then modifyPrecedenceTable (declareOperator defaultAssociativity
                                               defaultPrecedence
                                               qname)
   else return ()
  where uname = unqualifiedName qname

isOperatorPartM :: QName -> M Bool
isOperatorPartM qname = do
  table <- getPrecedenceTable
  return $ isOperatorPart qname table

isLeftAssocM :: QName -> M Bool
isLeftAssocM op = do
  table <- getPrecedenceTable
  case operatorAssociativity op table of
    LeftAssoc -> return True
    _         -> return False

isRightAssoc :: QName -> PrecedenceTable -> Bool
isRightAssoc op table =
  case operatorAssociativity op table of
    RightAssoc -> True
    _          -> False

isToplevelScopeM :: M Bool
isToplevelScopeM = do
  state <- getFS
  return $ stateScopeLevel state == 0

enterScopeM :: M ()
enterScopeM = modifyFS (\ state ->
              state { stateScopeLevel = stateScopeLevel state + 1 })

exitScopeM :: M ()
exitScopeM = modifyFS (\ state ->
              state { stateScopeLevel = stateScopeLevel state - 1 })

---- Parser

parseM :: M Program
parseM = do
  -- Initialize primitive module
  enterModule modulePRIM
  exportAllNamesFromModuleM modulePRIM
  declareQNameM primitiveUnderscore
  declareQNameM primitiveMain
  declareQNameM primitiveType

  declareQNameM primitiveTensorElim
  declareQNameM primitiveWithElim1
  declareQNameM primitiveWithElim2
  declareQNameM primitiveLinSumIntro1
  declareQNameM primitiveLinSumIntro2
  declareQNameM primitiveLinSumElim
  declareQNameM primitiveBangElim

  declareOperatorM RightAssoc 30 primitiveForall
  declareOperatorM RightAssoc 40 primitiveColon
  declareOperatorM RightAssoc 50 primitiveArrow
  declareOperatorM LeftAssoc 60 primitiveTensor
  declareOperatorM LeftAssoc 60 primitiveWith
  declareOperatorM LeftAssoc 60 primitiveLinSum
  declareOperatorM LeftAssoc 60 primitiveWithIntro
  declareOperatorM RightAssoc 70 primitiveBang

  -- Parse
  decls <- parseModules 
  return $ Program {
             programDeclarations = decls
           }

parseModules :: M [Declaration]
parseModules = do
  t <- peekType
  case t of
    T_Module -> do ds1 <- parseModule
                   ds2 <- parseModules
                   return (ds1 ++ ds2)
    T_LBrace -> do ds1 <- parseBareModule
                   ds2 <- parseModules
                   return (ds1 ++ ds2)
    T_EOF    -> return []
    _        -> failM ParseErrorExpectedModule
                      ("Expected a module, but got: " ++ show t ++ ".")

parseModule :: M [Declaration]
parseModule = do
  match T_Module
  qname <- parseQName
  enterModule qname
  importAllNamesFromModuleM modulePRIM -- Every module imports PRIM
  parseModuleExports qname
  match T_Where
  match T_LBrace
  decls <- parseDeclarations
  match T_RBrace
  return decls

parseBareModule :: M [Declaration]
parseBareModule = do
  enterModule moduleMain
  exportAllNamesFromModuleM moduleMain
  importAllNamesFromModuleM modulePRIM -- Every module imports PRIM
  match T_LBrace
  decls <- parseDeclarations
  match T_RBrace
  return decls

parseSequence :: M Bool -> M () -> M a -> M [a]
parseSequence checkTermination matchSeparator parseElement = do
    b <- checkTermination
    if b
     then return []
     else rec
  where
    rec = do
      elem  <- parseElement
      b     <- checkTermination
      if b
       then return [elem]
       else do matchSeparator
               elems <- rec
               return (elem : elems)

-- Parses a possibly empty sequence x1; 
parseSuite :: M a -> M [a]
parseSuite = parseSequence (peekIs T_RBrace) (match T_Semicolon)

parseModuleExports :: QName -> M ()
parseModuleExports moduleName = do
  t <- peekType
  case t of
    T_LParen -> do
      match T_LParen
      exportedNames <- parseSequence (peekIs T_RParen)
                                     (match T_Semicolon)
                                     parseId 
      match T_RParen
      exportNamesM moduleName exportedNames
    _ ->
      exportAllNamesFromModuleM moduleName

parseQName :: M QName
parseQName = do
  id  <- parseId
  t <- peekType
  case t of
    T_Dot -> do
      match T_Dot
      qname <- parseQName
      return $ Qualified id qname
    _ ->
      return $ readName id

resolveQName :: QName -> M QName
resolveQName qname = do
  state <- getFS
  case resolveName (stateRootModule state) (stateNameContext state) qname of
    Left  errmsg -> failM ModuleSystemError errmsg
    Right qname' -> return qname'

parseAndResolveQName :: M QName
parseAndResolveQName = do
  qname <- parseQName
  resolveQName qname

peekAndResolveQName :: M (Maybe QName)
peekAndResolveQName = do
  t <- peekType
  case t of
    T_Id _ -> do state <- getFS
                 qname <- parseAndResolveQName
                 putFS state
                 return $ Just qname
    _ -> return Nothing

parseId :: M String
parseId = do
  t <- peekType
  case t of
    T_Id id -> do getToken
                  return id
    _       -> failM ParseErrorExpectedId
                     ("Expected an identifier.\n" ++
                      "Got: " ++ show t ++ ".")

parseAndResolveId :: M QName
parseAndResolveId = do
  id <- parseId
  resolveQName (Name id)

parseInt :: M Integer
parseInt = do
  t <- peekType
  case t of
    T_Int n -> do getToken
                  return n
    _       -> failM ParseErrorExpectedInt
                     ("Expected an integer.\n" ++
                      "Got: " ++ show t ++ ".")

parseDeclarations :: M [Declaration]
parseDeclarations = do
  dss <- parseSuite parseDeclaration
  return $ concat dss

parseDeclaration :: M [Declaration]
parseDeclaration = do
  isTop <- isToplevelScopeM
  if isTop
   then parseToplevelDeclaration
   else do d <- parseTypeSignatureOrValueDeclaration
           return [d]

parseToplevelDeclaration :: M [Declaration]
parseToplevelDeclaration = do
  t <- peekType
  case t of
    T_Import   -> do parseImport
                     return []
    T_Infix    -> do parseFixityDeclaration NonAssoc
                     return []
    T_Infixl   -> do parseFixityDeclaration LeftAssoc
                     return []
    T_Infixr   -> do parseFixityDeclaration RightAssoc
                     return []
    T_Type     -> do d <- parseTypeDeclaration
                     return [d]
    T_Data     -> do d <- parseDataDeclaration
                     return [d]
    T_Class    -> do d <- parseClassDeclaration
                     return [d]
    T_Instance -> do d <- parseInstanceDeclaration
                     return [d]
    T_Mutual   -> do d <- parseMutualDeclaration
                     return [d]
    T_Eval     -> do d <- parseEvalDeclaration
                     return [d]
    _          -> do d <- parseTypeSignatureOrValueDeclaration
                     return [d]

parseImport :: M ()
parseImport = do
  match T_Import
  moduleName <- parseQName
  t <- peekType
  case t of
    T_LParen -> do match T_LParen
                   renamings  <- parseSequence (peekIs T_RParen)
                                               (match T_Semicolon)
                                               parseRenameId
                   importNamesM moduleName renamings
                   match T_RParen
    _ -> importAllNamesFromModuleM moduleName
  -- Parse optional alias for module
  t' <- peekType
  case t' of
    T_As -> do match T_As
               alias <- parseId
               declareModuleAliasM moduleName alias
    _ -> return ()

parseRenameId :: M (String, String)
parseRenameId = do
  id <- parseId
  t <- peekType
  case t of
    T_As -> do match T_As
               alias <- parseId
               return (id, alias)
    _ -> return (id, id)

parseTypeDeclaration :: M Declaration
parseTypeDeclaration = do
  match T_Type
  pos <- currentPosition
  expr1 <- parseExpr
  case exprHeadVariable expr1 of
    Just name -> declareQNameM name
    Nothing   -> failM ParseErrorTypeHasNoHead
                       ("Type name has no head variable: " ++ show expr1)
  match T_Eq
  expr2 <- parseExpr
  return $ TypeDeclaration pos expr1 expr2

parseDataDeclaration :: M Declaration
parseDataDeclaration = do
  match T_Data
  pos <- currentPosition
  expr <- parseExpr
  case exprHeadVariable expr of
    Just name -> declareQNameM name
    Nothing   -> failM ParseErrorDataHasNoHead
                       ("Type name has no head variable: " ++ show expr)
  match T_Where
  match T_LBrace
  constructors <- parseSignatures
  match T_RBrace
  return $ DataDeclaration pos expr constructors

parseClassDeclaration :: M Declaration
parseClassDeclaration = do
  match T_Class
  pos       <- currentPosition
  className <- parseAndResolveQName
  declareQNameM className
  typeName  <- parseAndResolveQName
  match T_Where
  match T_LBrace
  signatures <- parseSignatures
  match T_RBrace
  return $ ClassDeclaration pos className typeName signatures

parseSignatures :: M [Signature]
parseSignatures = parseSuite parseSignature

parseSignature :: M Signature
parseSignature = do
  pos  <- currentPosition
  name <- parseAndResolveQName
  declareQNameM name
  matchColon
  typ <- parseExpr
  constraints <- parseOptionalConstraints
  return $ Signature pos name typ constraints

parseInstanceDeclaration :: M Declaration
parseInstanceDeclaration = do
  match T_Instance
  pos         <- currentPosition
  className   <- parseAndResolveQName
  typ         <- parseExpr
  constraints <- parseOptionalConstraints
  match T_Where
  match T_LBrace
  equations <- parseEquations
  match T_RBrace
  return $ InstanceDeclaration pos className typ constraints equations

parseEquations :: M [Equation]
parseEquations = parseSuite parseEquation

parseEquation :: M Equation
parseEquation = do
  pos <- currentPosition
  lhs <- parseExpr
  case exprHeadVariable lhs of
    Just qname -> declareQNameM qname
    Nothing    -> failM ParseErrorEquationHasNoHead
                        ("Left-hand side of equation has no head variable: " ++
                         show lhs)
  match T_Eq
  rhs <- parseExpr
  t   <- peekType
  case t of
    T_Where -> do
      enterScopeM
      match T_Where
      match T_LBrace
      decls <- parseDeclarations
      match T_RBrace
      exitScopeM
      return $ Equation pos lhs (ELet pos decls rhs)
    _       -> return $ Equation pos lhs rhs

parseOptionalConstraints :: M [Constraint]
parseOptionalConstraints = do
  t <- peekType
  case t of
    T_LBrace -> do
      match T_LBrace
      constraints <- parseConstraints
      match T_RBrace
      return constraints
    _        -> return []

parseConstraints :: M [Constraint]
parseConstraints = parseSuite parseConstraint

parseConstraint :: M Constraint
parseConstraint = do
  pos       <- currentPosition
  className <- parseAndResolveQName
  typeName  <- parseAndResolveQName
  return $ Constraint pos className typeName

parseFixityDeclaration :: Associativity -> M ()
parseFixityDeclaration assoc = do
  getToken
  precedence    <- parseInt
  operatorName  <- parseId
  currentModule <- getCurrentModuleName 
  declareOperatorM assoc precedence (qualify currentModule operatorName)

parseMutualDeclaration :: M Declaration
parseMutualDeclaration = do
  match T_Mutual
  pos          <- currentPosition
  match T_LBrace
  enterScopeM
  declarations <- parseDeclarations
  exitScopeM
  match T_RBrace
  return $ MutualDeclaration pos declarations

parseEvalDeclaration :: M Declaration
parseEvalDeclaration = do
  match T_Eval
  pos  <- currentPosition
  expr <- parseExpr
  return $ EvalDeclaration pos expr

parseTypeSignatureOrValueDeclaration :: M Declaration
parseTypeSignatureOrValueDeclaration = do
  t <- peekType
  case t of
    T_Id _ -> do state <- getFS
                 parseAndResolveQName -- skip name
                 t' <- peekType
                 putFS state
                 case t' of
                   T_Id c | c == colonSymbol -> parseTypeSignature
                   _                         -> parseValueDeclaration
    _ -> parseValueDeclaration

parseTypeSignature :: M Declaration
parseTypeSignature = do
  sig <- parseSignature
  return $ TypeSignature sig

parseValueDeclaration :: M Declaration
parseValueDeclaration = do
  equation <- parseEquation 
  return $ ValueDeclaration equation

parseExpr :: M Expr
parseExpr = do
  t <- peekType
  case t of
    T_Let    -> parseLet
    T_Lambda -> parseLambda
    T_Fresh  -> parseFresh
    T_Case   -> parseCase
    _        -> do
      table <- getPrecedenceTable
      parseExprMixfix (precedenceTableLevels table) table

parseLambda :: M Expr
parseLambda = do
  pos <- currentPosition
  match T_Lambda
  params <- parseSequence checkArrow (return ()) parseAtom
  matchArrow
  body <- parseExpr
  return $ foldr (ELambda pos) body params

parseFresh :: M Expr
parseFresh = do
  pos <- currentPosition
  match T_Fresh
  match T_LBrace
  freshNames <- parseSuite parseQNames
  match T_RBrace
  match T_In
  expr <- parseExpr
  return $ foldr (EFresh pos) expr (concat freshNames)

parseQNames :: M [QName]
parseQNames = parseSequence (peekIsAny [T_Semicolon, T_RBrace])
                            (return ())
                            parseAndResolveQName

matchArrow :: M ()
matchArrow = match (T_Id arrowSymbol)

matchColon :: M ()
matchColon = match (T_Id colonSymbol)

checkArrow :: M Bool
checkArrow = do
  t <- peekType
  case t of
    T_Id id -> return (id == arrowSymbol)
    _       -> return False

parseLet :: M Expr
parseLet = do
  pos <- currentPosition
  enterScopeM
  match T_Let
  match T_LBrace
  decls <- parseDeclarations
  match T_RBrace
  match T_In
  expr <- parseExpr
  exitScopeM
  return $ ELet pos decls expr

parseCase :: M Expr
parseCase = do
  pos <- currentPosition
  match T_Case
  expr <- parseExpr
  match T_Of
  match T_LBrace
  branches <- parseSuite parseCaseBranch
  match T_RBrace
  return $ ECase pos expr branches

parseCaseBranch :: M CaseBranch
parseCaseBranch = do
  pos <- currentPosition
  pattern <- parseAtom
  matchArrow
  result <- parseExpr
  return $ CaseBranch pos pattern result

isEndOfExpression :: M Bool
isEndOfExpression = do
  t <- peekType
  return $ case t of
    T_EOF       -> True
    T_RParen    -> True
    T_LBrace    -> True
    T_RBrace    -> True
    T_Semicolon -> True
    T_Eq        -> True
    T_Of        -> True
    T_Where     -> True
    _           -> False

{-- Mixfix parser --}

data Status = EmptyStatus Position
            | PushArgument Status Expr
            | PushOperatorPart Status QName

instance Show Status where
  show (EmptyStatus _) = ""
  show (PushOperatorPart status p) = show status ++ show p
  show (PushArgument status x)     = show status ++ "(" ++ show x ++ ")"

parseExprMixfix :: [PrecedenceLevel] -> PrecedenceTable -> M Expr
parseExprMixfix []     _     = parseApplication
parseExprMixfix levels table = do
  pos <- currentPosition
  parseExprInfix levels table (EmptyStatus pos)

parseExprInfix :: [PrecedenceLevel] -> PrecedenceTable -> Status -> M Expr
parseExprInfix []                      _     _      =
  error "parseExprInfix: list of levels cannot be empty"
parseExprInfix (currentLevel : levels) table status = do
   b <- isEndOfExpression
   if b
    then prematureEndOfExpression status
    else do
      b <- mustReadPart currentLevel table status
      if b
       then do
         part <- peekAndResolveQName >>= fromJustOrFail
         isOp <- isOperatorPartM part
         if isOp && statusIsValidPrefixInLevel
                        (PushOperatorPart status part)
                        currentLevel
          then do parseAndResolveQName
                  rec (PushOperatorPart status part)
          else prematureEndOfExpression status
       else do arg <- parseExprMixfix levels table
               rec (PushArgument status arg)
  where
    -- If the does not continue with an operator, there are two cases:
    --   If there is exactly one pushed argument, return it.
    --   Otherwise, fail.
    prematureEndOfExpression (PushArgument (EmptyStatus _) x) = return x
    prematureEndOfExpression _ =
      do t <- peekType
         failM ParseErrorPrematureEndOfExpression
           ("Premature end of expression.\n" ++
            "Possibly expected operator part.\n" ++
            "Got: " ++ show t ++ ".\n" ++
            "Status: " ++ show status)

    -- Check whether we have to read an operator part.
    mustReadPart :: PrecedenceLevel -> PrecedenceTable -> Status -> M Bool
    mustReadPart level table status@(EmptyStatus _) = do
      maybeQName <- peekAndResolveQName
      case maybeQName of
        Nothing    -> return False
        Just qname -> do
          isOp <- isOperatorPartM qname
          return $ isOp && statusIsValidPrefixInLevel
                             (PushOperatorPart status qname)
                             level
    mustReadPart _ _ (PushArgument _ _)     = return True
    mustReadPart _ _ (PushOperatorPart _ _) = return False

    -- Continue reading the expression recursively.
    -- If we are finished, return.
    rec status'
      -- Case 1: "Almost finished" right associative operator
      --         (exactly one argument pending)
      | statusIsAlmostCompleteRightOperator status' currentLevel
        = do -- Parse last argument *on the same level*
             arg <- parseExprMixfix (currentLevel : levels) table
             rec (PushArgument status' arg)

      -- Case 2: Finished operator
      | statusIsCompleteOperator status' currentLevel
        = let op = statusOperatorName status'
              pos = statusPosition status'
              result = foldr (flip (EApp pos))
                             (EVar pos op)
                             (statusArguments status')
            in do b <- isLeftAssocM op
                  if b
                   then -- Left-associative operator:
                        -- restart on the same level with the
                        -- accumulated expression.
                        parseExprInfix
                          (currentLevel : levels)
                          table
                          (PushArgument (EmptyStatus pos) result)
                   else -- Non-associative or right-associative operator
                        return result
      -- Case 3: Unfinished
      | otherwise =
          parseExprInfix (currentLevel : levels) table status'


    -- Check if status is an "almost complete" right-associative operator
    -- (exactly one operator pending).
    statusIsAlmostCompleteRightOperator :: Status -> PrecedenceLevel -> Bool
    statusIsAlmostCompleteRightOperator status level =
      let moduleName = statusModuleName status
          parts      = statusOperatorParts status
       in case moduleName of
            Nothing -> False
            Just m  ->
              let fullOperatorName = qualify m (concat (parts ++ ["_"])) in
                any (operatorIsEqual moduleName (parts ++ ["_"]))
                    (S.toList level)
                &&
                isRightAssoc fullOperatorName table

    -- Check if status is a complete operator
    statusIsCompleteOperator :: Status -> PrecedenceLevel -> Bool
    statusIsCompleteOperator status level =
      let moduleName = statusModuleName status
          parts      = statusOperatorParts status
       in any (operatorIsEqual moduleName parts)
              (S.toList level)

    fromJustOrFail :: Maybe a -> M a
    fromJustOrFail Nothing = do
      state <- getFS
      failM InternalError "INTERNAL ERROR: Extend isEndOfExpression ?"
    fromJustOrFail (Just x) = return x

    statusPosition :: Status -> Position
    statusPosition (EmptyStatus pos)           = pos
    statusPosition (PushArgument status _)     = statusPosition status
    statusPosition (PushOperatorPart status _) = statusPosition status

    statusIsValidPrefixInLevel :: Status -> PrecedenceLevel -> Bool
    statusIsValidPrefixInLevel (EmptyStatus _)                  _ = True
    statusIsValidPrefixInLevel (PushArgument (EmptyStatus _) _) _ = True
    statusIsValidPrefixInLevel status level =
      let moduleName = statusModuleName status
          parts      = statusOperatorParts status
       in (case moduleName of
             Nothing -> True
             Just m  -> statusModuleNamesCoherent m status) &&
          any (operatorIsPrefix moduleName parts) (S.toList level)

    statusModuleName :: Status -> Maybe QName
    statusModuleName (EmptyStatus _)        = Nothing
    statusModuleName (PushOperatorPart _ q) = Just $ moduleNameFromQName q
    statusModuleName (PushArgument s _)     = statusModuleName s

    statusModuleNamesCoherent :: QName -> Status -> Bool
    statusModuleNamesCoherent m (EmptyStatus _) = True
    statusModuleNamesCoherent m (PushOperatorPart _ q) =
      m == moduleNameFromQName q
    statusModuleNamesCoherent m (PushArgument s _) =
      statusModuleNamesCoherent m s

    statusOperatorParts :: Status -> [String]
    statusOperatorParts (EmptyStatus _)        = [] 
    statusOperatorParts (PushOperatorPart s q) =
      statusOperatorParts s ++ [unqualifiedName q]
    statusOperatorParts (PushArgument s _)     =
      statusOperatorParts s ++ ["_"]

    statusOperatorName :: Status -> QName
    statusOperatorName status =
      case statusModuleName status of
        Nothing -> error "Status does not match an operator."
        Just m  -> qualify m (concat (statusOperatorParts status))

    statusArguments :: Status -> [Expr]
    statusArguments (EmptyStatus _)             = []
    statusArguments (PushOperatorPart status _) = statusArguments status
    statusArguments (PushArgument status arg)   = arg : statusArguments status

    operatorMatches :: ([String] -> [String] -> Bool)
                    -> Maybe QName -> [String] -> QName -> Bool
    operatorMatches f moduleName parts targetOperator =
        let targetOperatorModule = moduleNameFromQName targetOperator
            targetOperatorParts  = splitParts (unqualifiedName targetOperator)
         in (case moduleName of
              Nothing -> True
              Just m  -> m == targetOperatorModule) &&
            f parts targetOperatorParts

    operatorIsPrefix :: Maybe QName -> [String] -> QName -> Bool
    operatorIsPrefix = operatorMatches isPrefixOf

    operatorIsEqual :: Maybe QName -> [String] -> QName -> Bool
    operatorIsEqual = operatorMatches (==)

{-- End mixfix parser --}

parseApplication :: M Expr
parseApplication = do
  pos <- currentPosition
  head <- parseAtom
  args <- parseApplicationArguments
  return $ foldl (EApp pos) head args

parseApplicationArguments :: M [Expr]
parseApplicationArguments = do
  isEnd <- isEndOfApplication
  if isEnd
   then return []
   else do arg  <- parseAtom
           args <- parseApplicationArguments
           return (arg : args)

isEndOfApplication :: M Bool
isEndOfApplication = do
  isEnd <- isEndOfExpression
  if isEnd
   then return True
   else do maybeQName <- peekAndResolveQName
           case maybeQName of
             Nothing    -> return False
             Just qname -> isOperatorPartM qname

parseAtom :: M Expr
parseAtom = do
  t <- peekType
  case t of
    T_Id _      -> do pos   <- currentPosition
                      qname <- parseAndResolveQName
                      isOp  <- isOperatorPartM qname 
                      if isOp
                        then failM ParseErrorOperatorPartUsedAsVariable
                                  ("Operator part: " ++ show qname ++
                                    " cannot be used as a variable.")
                        else return $ EVar pos qname
    T_Dot       -> do pos   <- currentPosition
                      match T_Dot
                      qname <- parseAndResolveId
                      return $ EUnboundVar pos qname
    T_Int n     -> do pos <- currentPosition
                      getToken
                      return $ EInt pos n
    T_Char c    -> do pos <- currentPosition
                      getToken
                      return $ EChar pos c
    T_String c  -> do pos <- currentPosition
                      getToken
                      return $ EString pos c
    T_LParen    -> do pos <- currentPosition
                      match T_LParen
                      t <- peekType
                      case t of
                        T_RParen  -> do match T_RParen
                                        return $ EVar pos primitiveUnit
                        _         -> do expr <- parseExpr
                                        match T_RParen
                                        return expr
    _           -> failM ParseErrorExpectedExpression
                        ("Expected an expression.\n" ++
                          "Got: " ++ show t ++ ".")

