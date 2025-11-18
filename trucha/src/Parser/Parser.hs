
module Parser.Parser(parse) where

import qualified Data.Set as S

import Utils(joinS, dropLast)
import FailState(
         ErrMsg, FailState, evalFS, getFS, putFS, modifyFS, failFS, logFS,
         tryOrBacktrackFS
       )
import Parser.Token(Token(..), TokenType(..))
import Syntax.Name(
         ModuleName, rootModuleName, moduleNameFromString,
         makeModuleName, moduleNameParts,
         QName, makeQName, qnameBareName, qnameModuleName, isBareQName,
         qnameIsOperator, BareName, makeBareName, bareNameAllParts,
         makeInternalQName, HoleName(..),
         --
         globalNameType, globalNameUnderscore, arrowSymbol, arrowOperator,
         arrowFunction, timesSymbol, timesOperator, timesFunction
       )
import Syntax.AST(
         AProgram(..), ADeclaration(..), AParameter(..),
         AConstructorDecl(..),
         AExpr(..), appMany, appManyOpt, lamManyParams, patternHead,
         AOptionalExpr(..),
         ACaseBranch(..), APattern(..),
         Program, Declaration, Parameter, ConstructorDecl,
         Expr, OptionalExpr, CaseBranch, Pattern,
         annotation,
         exprToPattern
       )
import qualified Syntax.ModuleSystem as MS
import qualified Syntax.Position as Pos
import qualified Parser.PrecedenceTable as PT

parse :: [Token] -> Either ErrMsg Program
parse input = evalFS initialState (do initialize
                                      parseProgram)
  where initialState = ParserState {
                         moduleSystem      = MS.initialRootModule
                       , currentModuleName = rootModuleName
                       , precedenceTable   = PT.emptyPrecedenceTable
                       , prev              = []
                       , next              = input
                       , nextFreshVar      = 0
                       }

data ParserState = ParserState {
                     moduleSystem      :: MS.ModuleSystem
                   , currentModuleName :: ModuleName
                   , precedenceTable   :: PT.PrecedenceTable
                   , prev              :: [Token]
                   , next              :: [Token]
                   , nextFreshVar      :: Integer
                   }
type M = FailState ParserState

-----

-- Initialize global names and operators

initialize :: M ()
initialize = do
  registerLocalQName globalNameType
  declareOperator PT.AssocRight (PT.Precedence 0) arrowFunction
  registerLocalQName arrowFunction
  declareOperator PT.AssocRight (PT.Precedence 2) timesFunction
  registerLocalQName timesFunction

reservedWords :: S.Set QName
reservedWords = S.fromList [
                  arrowOperator
                , timesOperator
                ]

-----

parseProgram :: M Program
parseProgram = do
  decls <- parseDeclarationBlock
  return $ Program decls

parseDeclarations :: M [Declaration]
parseDeclarations = do
  tokTyp <- peekType
  case tokTyp of
    TEof    -> return []
    _       -> do ds1 <- parseDeclaration
                  ds2 <- parseDeclarations
                  return (ds1 ++ ds2)

parseDeclarationBlock :: M [Declaration]
parseDeclarationBlock =
  parseDelimitedSequenceL TLBrace TSemicolon TRBrace parseDeclaration

parseDeclaration :: M [Declaration]
parseDeclaration = do
    tokTyp <- peekType
    case tokTyp of
      TModule -> parseModuleDeclaration
      TImport -> parseImportDeclaration
      TInfix  -> parseFixityDeclaration
      TInfixL -> parseFixityDeclaration
      TInfixR -> parseFixityDeclaration
      TData   -> parseDataDeclaration
      THole _ -> parseCheckDeclaration
      _ -> do
        (tokTyp1, tokTyp2) <- peek2Types
        if isTId tokTyp1 && tokTyp2 == TColon
         then parseDeclType
         else parseDeclEquation

parseModuleDeclaration :: M [Declaration]
parseModuleDeclaration = do
  match TModule
  moduleNameSuffix <- parseModuleName
  exports <- parseModuleExports
  match TWhere
  enterModule moduleNameSuffix exports
  decls <- parseDeclarationBlock
  exitModule moduleNameSuffix
  return decls

parseImportDeclaration :: M [Declaration]
parseImportDeclaration = do
  match TImport
  importedModuleName <- parseModuleName
  imports <- parseModuleImports importedModuleName
  moduleImport importedModuleName imports
  return []

parseFixityDeclaration :: M [Declaration]
parseFixityDeclaration = do
    associativity <- parseAssociativity
    precedence <- parsePrecedence
    bareName <- parseBareName
    if length (bareNameAllParts bareName) < 2
     then failM "Operator name must contain at least one placeholder (\"_\")."
     else return ()
    mod <- currentModuleName <$> getFS
    let qname = makeQName mod bareName
    declareOperator associativity precedence qname
    registerLocalQName qname
    return []
  where
    parseAssociativity :: M PT.Associativity
    parseAssociativity = do
      tokTyp <- peekType
      associativity <-
        case tokTyp of
          TInfix  -> return PT.NonAssoc
          TInfixL -> return PT.AssocLeft
          TInfixR -> return PT.AssocRight
          _ -> failM "Expected a fixity declaration."
      nextToken
      return associativity
    parsePrecedence :: M PT.Precedence
    parsePrecedence = PT.Precedence <$> parseInteger

parseDataDeclaration :: M [Declaration]
parseDataDeclaration = do
  pos <- getPos
  match TData
  bareName <- parseBareName 
  qname <- resolveNameOrRegister (makeQName rootModuleName bareName)
  parameters <- parseDataDeclParameters 
  tokTyp <- peekType
  if tokTyp `notElem` [TColon, TWhere]
   then failM "Expected \":\" or \"where\" to complete data declaration."
   else return ()
  declDataType <- do
    tokTyp <- peekType
    case tokTyp of 
      TColon -> do match TColon
                   typ <- parseExpr
                   return [DeclDataType pos qname parameters typ]
      _      -> return []
  declDataConstructors <- do
    tokTyp <- peekType
    tokTyp <- peekType
    case tokTyp of 
      TWhere -> do match TWhere
                   ctors <- parseDelimitedSequenceL TLBrace TSemicolon TRBrace
                                                    parseConstructorDecl
                   return [DeclDataConstructors pos qname parameters ctors]
      _      -> return []
  return (declDataType ++ declDataConstructors)

parseCheckDeclaration :: M [Declaration]
parseCheckDeclaration = do
  pos <- getPos
  match $ THole (HoleName "")
  expr <- parseExpr
  return [DeclCheck pos expr]

parseDataDeclParameters :: M [Parameter]
parseDataDeclParameters = do
  tokTyp <- peekType
  if isTId tokTyp || tokTyp == TLParen || tokTyp == TLBrace
   then do ps1 <- parseDataDeclParameter
           ps2 <- parseDataDeclParameters
           return (ps1 ++ ps2)
   else return []

parseDataDeclParameter :: M [Parameter]
parseDataDeclParameter = do
  pos <- getPos
  tokTyp <- peekType
  case tokTyp of
    TId qname -> do
      name <- parseParameterName
      return [ExplicitParameter pos name (EWildcard pos)]
    TLParen -> do
      match TLParen
      names <- parseSequenceUntil [TColon] parseParameterName
      match TColon
      typ <- parseExpr
      match TRParen
      return $ map (\ name -> ExplicitParameter pos name typ) names
    TLBrace -> do
      match TLBrace
      names <- parseSequenceUntil [TColon, TRBrace] parseParameterName
      tokTyp' <- peekType 
      typs <- if tokTyp' == TColon
               then do match TColon
                       expr <- parseExpr
                       return [expr | _ <- names]
               else return [EWildcard pos | _ <- names]
      match TRBrace
      return $ zipWith (\ name typ -> ImplicitParameter pos name typ) names typs
    _ -> failM "Expected a parameter."

parseParameterName :: M QName
parseParameterName = do
  tokTyp <- peekType
  case tokTyp of
    TUnderscore -> do
      nextToken
      return globalNameUnderscore
    TId qname | qname `S.notMember` reservedWords -> do
      nextToken
      resolveNameOrReturnLocalName qname               
    _ -> failM ("Expected a parameter name. Found: " ++ show tokTyp)

parseConstructorDecl :: M [ConstructorDecl]
parseConstructorDecl = do
  pos <- getPos
  constructorBareName <- parseBareName
  constructorQName <- registerLocalName constructorBareName
  match TColon
  typ <- parseExpr
  return [ConstructorDecl pos constructorQName typ]

parseInteger :: M Integer
parseInteger = do
  tokTyp <- peekType
  case tokTyp of
    TInt n -> do nextToken
                 return n
    _ -> failM ("Expected an integer; got: " ++ show tokTyp)

parseDeclType :: M [Declaration]
parseDeclType = do
  pos <- getPos
  bareName <- parseBareName
  match TColon
  typ <- parseExpr
  qname <- registerLocalName bareName
  return [DeclType pos qname typ]

parseDeclEquation :: M [Declaration]
parseDeclEquation = do
  pos <- getPos
  mod <- currentModuleName <$> getFS
  lhsExpr <- parseExpr
  lhs <- case exprToPattern lhsExpr of
           Nothing  -> failM "Left-hand side of equation must be a pattern."
           Just pat -> return pat
  let lhsHead = patternHead lhs
  case lhsHead of
    PVar _ qname | qnameModuleName qname == mod -> do
      registerLocalQName qname
    _ -> failM (
            "Head of left-hand side of equation must be a local name.\n"
         ++ "Found: " ++ show lhsHead
         )
  match TEq
  rhs <- parseExpr
  return [DeclEquation pos lhs rhs]

parseModuleExports :: M MS.Exports
parseModuleExports = do
  tokTyp <- peekType 
  if tokTyp == TLParen
   then MS.ExportNames <$> parseDelimitedSequence TLParen TSemicolon TRParen
                                                  parseBareName
   else return MS.ExportAll

parseModuleImports :: ModuleName -> M MS.Imports
parseModuleImports importedModuleName = do
  tokTyp <- peekType 
  case tokTyp of
    TLParen ->
      MS.ImportNames <$> parseDelimitedSequence TLParen TSemicolon TRParen
                                                parseBareName
    TId qname | qname == kw_qualified -> do
      nextToken
      tokTyp2 <- peekType
      case tokTyp2 of
        TId qname | qname == kw_as -> do
          nextToken
          synonym <- parseBareName
          return $ MS.ImportQualified (makeModuleName [show synonym])
        _ -> return $ MS.ImportQualified importedModuleName
    _ ->
      return MS.ImportAll
  where
    kw name = makeQName rootModuleName (makeBareName [name])
    kw_qualified = kw "qualified"
    kw_as        = kw "as"

parseModuleName :: M ModuleName
parseModuleName = do
  tokTyp <- peekType
  case tokTyp of
    TId qname -> do nextToken
                    return $ moduleNameFromString (show qname)
    _ -> failM "Expected a module name."

parseBareName :: M BareName
parseBareName = do
  tokTyp <- peekType
  case tokTyp of
    TId qname ->
      if isBareQName qname
       then do nextToken
               return $ qnameBareName qname
       else failM ("Expected a bare name, but got: " ++ show qname)
    _ -> failM ("Expected a bare name, but got a " ++ show tokTyp)

parseExpr :: M Expr
parseExpr = do
  pt <- precedenceTable <$> getFS
  parseMixfixExpr (PT.allPrecedenceLevels pt)

data Arg = ArgPlaceholder Pos.Position Expr
         | ArgRigidPart Pos.Position QName

argPosition :: Arg -> Pos.Position
argPosition (ArgPlaceholder pos _) = pos
argPosition (ArgRigidPart pos _)   = pos

argToExprs :: Arg -> [Expr]
argToExprs (ArgPlaceholder _ expr) = [expr]
argToExprs (ArgRigidPart _ expr)   = []

isArgPlaceholder :: Arg -> Bool
isArgPlaceholder (ArgPlaceholder _ _) = True
isArgPlaceholder (ArgRigidPart _ _)   = False

instance Show Arg where
  show (ArgPlaceholder _ expr) = show expr
  show (ArgRigidPart _ qname)  = show qname

parseMixfixExpr :: [PT.PrecedenceLevel] -> M Expr
parseMixfixExpr [] = parseInnerExpr
parseMixfixExpr allLevels@(PT.PrecedenceLevel associativity allOperatorsAtThisLevel
                           : lowerLevels) = do
    rec (S.toList allOperatorsAtThisLevel) []
  where
    rec :: [PT.Operator] -> [Arg] -> M Expr
    rec oprs args = do
        pos <- getPos
        tokTyp <- peekType
        case tokTyp of
          TId qname -> do
            qname' <- resolveNameOrReturnLocalName qname
            let continuations = [
                    PT.Operator fullOp opRest
                  | PT.Operator fullOp (PT.OprPart opPart : opRest) <- oprs,
                    opPart == qname'
                  ]
            if null continuations
             then tryContinue oprs args
             else do nextToken
                     completeOrProceed continuations
                                       (args ++ [ArgRigidPart pos qname'])
          _ -> tryContinue oprs args
      where
        tryContinue :: [PT.Operator] -> [Arg] -> M Expr
        tryContinue oprs args = do
          case args of
            [ArgPlaceholder _ x] -> return x
            _ -> recInfix oprs args
    recInfix :: [PT.Operator] -> [Arg] -> M Expr
    recInfix oprs args = do
      pos <- getPos
      let continuations = [
              PT.Operator fullOp opRest
            | PT.Operator fullOp (PT.OprPlaceholder : opRest) <- oprs
            ]
      let isLastArg = not (null continuations) &&
                      all (\ (PT.Operator _ opRest) -> null opRest) continuations
      let levels = if isLastArg && PT.isAssocRight associativity
                    then allLevels
                    else lowerLevels
      arg <- parseMixfixExpr levels
      completeOrProceed continuations (args ++ [ArgPlaceholder pos arg])
    completeOrProceed :: [PT.Operator] -> [Arg] -> M Expr
    completeOrProceed continuations args = do
      if null continuations
       then completeSingleArgument args
       else do
         let completedOps = [fullOp | PT.Operator fullOp [] <- continuations]
         if null completedOps
          then rec continuations args -- proceed
          else if length completedOps > 1
                then failM $ unlines ([
                       "Ambiguous use of operator. Possible operators:"
                     ] ++ map show completedOps)
                else completeOperatorApplication completedOps args
    completeSingleArgument :: [Arg] -> M Expr 
    completeSingleArgument args =
      case args of
       [ArgPlaceholder _ x] -> return x
       _ -> failM (
              "Badly formed mixfix expression. Partially read:"
              ++ joinS " " (map show args)
            )
    completeOperatorApplication :: [QName] -> [Arg] -> M Expr
    completeOperatorApplication completedOps args = do
      let pos = argPosition (head args)
      finalExpr <- applyOperator pos (head completedOps)
                                     (concatMap argToExprs args)
      if PT.isAssocLeft associativity
         && isArgPlaceholder (head args)
       then let continuations = [
                    PT.Operator fullOp opRest
                  | PT.Operator fullOp (PT.OprPlaceholder : opRest)
                    <- S.toList allOperatorsAtThisLevel
                  ]
              in rec continuations [ArgPlaceholder pos finalExpr]
       else return finalExpr 
    applyOperator :: Pos.Position -> QName -> [Expr] -> M Expr
    applyOperator pos opName args
      | opName == arrowFunction = do
          let [e1, e2] = args
          return $ EPi pos (ExplicitParameter pos globalNameUnderscore e1) e2
      | opName == timesFunction = do
          let [e1, e2] = args
          return $ ESig pos (ExplicitParameter pos globalNameUnderscore e1) e2
      | otherwise = return $ appMany (EVar pos opName) args

parseInnerExpr :: M Expr
parseInnerExpr = do
    exprs <- parseAtomicExprSequence1
    case head exprs of
      OptPlain fun -> return $ appManyOpt fun (tail exprs)
      OptOptional _ ->
        failM "Head of application cannot be surrounded by braces."
  where
    parseAtomicExprSequence1 :: M [OptionalExpr]
    parseAtomicExprSequence1 = do
      optExpr <- parseAtomicExpr
      tokTyp <- peekType
      b <- isTerminator tokTyp
      if b
       then return [optExpr]
       else do optExprs <- parseAtomicExprSequence1
               return (optExpr : optExprs)
    isTerminator :: TokenType -> M Bool
    isTerminator (TId qname) = do
      -- Operator parts always terminate applications
      qname' <- resolveNameOrReturnLocalName qname
      pt <- precedenceTable <$> getFS
      return $ PT.isOperatorPart pt qname'
    isTerminator TRParen     = return True
    isTerminator TRBrace     = return True
    isTerminator TSemicolon  = return True
    isTerminator TEq         = return True
    isTerminator TWhere      = return True
    isTerminator TOf         = return True
    isTerminator _           = return False

parseAtomicExpr :: M OptionalExpr
parseAtomicExpr = do
  pos <- getPos
  tokTyp <- peekType
  case tokTyp of
    TUnderscore -> do
      nextToken
      return . OptPlain $ EWildcard pos
    THole hname -> do
      nextToken
      return . OptPlain $ EHole pos hname
    TId qname -> do
      qname' <- resolveNameOrReturnLocalName qname
      nextToken
      pt <- precedenceTable <$> getFS
      if PT.isOperatorPart pt qname'
       then failM ("Operator part: " ++ show qname'
                  ++ " cannot be used as a variable name.")
       else return . OptPlain $ EVar pos qname'
    TLParen -> do
      b <- checkIfParenthesizedTypeParameterStarts
      if b
       then OptPlain <$> parseDependentType
       else OptPlain <$> parseParenthesizedExpr
    TLBrace -> do
      b <- checkIfParenthesizedTypeParameterStarts
      if b
       then OptPlain <$> parseDependentType
       else OptOptional <$> parseBracketedExpr
    TForall -> OptPlain <$> parseDependentType
    TExists -> OptPlain <$> parseDependentType
    TLambda -> OptPlain <$> parseLambda
    TCase -> OptPlain <$> parseCase
    TDot -> do
      nextToken
      oExpr <- parseAtomicExpr
      case oExpr of
        OptPlain expr -> return . OptPlain $ EInaccessible pos expr
        OptOptional _ -> failM "Inaccessible pattern cannot be optional."
    _ -> failM ("Expected an expression. Got: " ++ show tokTyp)

-- Check whether the following tokens are of the form
--   LParen (TId _) ... (TId _) TColon
-- or
--   LBrace (TId _) ... (TId _) TColon
-- without consuming the tokens.
checkIfParenthesizedTypeParameterStarts :: M Bool
checkIfParenthesizedTypeParameterStarts = do
  followingTokens <- next <$> getFS
  let followingTokenTypes = map tokenType followingTokens
  return $
    head followingTokenTypes `elem` [TLParen, TLBrace]
    && let remainder = dropWhile isTParamName (tail followingTokenTypes)
        in not (null remainder) && head remainder == TColon

parseParenthesizedExpr :: M Expr
parseParenthesizedExpr = do
  match TLParen
  expr <- parseExpr
  match TRParen
  return expr

parseBracketedExpr :: M Expr
parseBracketedExpr = do
  pos <- getPos
  match TLBrace
  tokTyp <- peekType
  if tokTyp == TDot
   then do
     nextToken
     expr <- parseExpr
     match TRBrace
     return $ EInaccessible pos expr
   else do
     expr <- parseExpr
     match TRBrace
     return expr

parseDependentType :: M Expr
parseDependentType = do
  pos <- getPos
  b <- checkIfParenthesizedTypeParameterStarts
  (operators, parameters) <-
   if b
     then do parameters <- parseParenthesizedTypeParameters False
             return ([TId arrowOperator, TId timesOperator], parameters)
     else do tokTyp <- peekType
             case tokTyp of
               TForall -> do
                 match TForall
                 parameters <- parseParenthesizedTypeParameters True
                 return ([TId arrowOperator], parameters)
               TExists -> do
                 match TExists
                 parameters <- parseParenthesizedTypeParameters True
                 return ([TId timesOperator], parameters)
               _ -> failM "Expected a type"
  tokTyp <- peekType
  case tokTyp of
    _ | tokTyp `elem` operators && tokTyp == TId arrowOperator -> do
      match (TId arrowOperator)
      expr <- parseExpr
      return $ foldr (EPi pos) expr parameters
    _ | tokTyp `elem` operators && tokTyp == TId timesOperator -> do
      match (TId timesOperator)
      expr <- parseExpr
      return $ foldr (ESig pos) expr parameters
    _ -> failM ("Expected an operator (" ++ arrowSymbol
                ++ " or " ++ timesSymbol ++ ")")

parseParenthesizedTypeParameters :: Bool -> M [Parameter]
parseParenthesizedTypeParameters allowBareParameters = do
  tokTyp <- peekType
  if tokTyp `elem` [TLParen, TLBrace]
   then do ps1 <- parseParenthesizedTypeParameter allowBareParameters
           ps2 <- parseParenthesizedTypeParameters allowBareParameters
           return (ps1 ++ ps2)
   else if allowBareParameters && isTParamName tokTyp
         then do ps1 <- parseBareTypeParameter
                 ps2 <- parseParenthesizedTypeParameters allowBareParameters
                 return (ps1 ++ ps2)
         else return []

parseBareTypeParameter :: M [Parameter]
parseBareTypeParameter = do
  pos <- getPos
  name <- parseParameterName
  return [ExplicitParameter pos name (EWildcard pos)]

parseParenthesizedTypeParameter :: Bool -> M [Parameter]
parseParenthesizedTypeParameter allowBareParameters = do
  pos <- getPos
  tokTyp <- peekType
  (terminator, paramConstructor) <-
    case tokTyp of
      TLParen -> do
        match TLParen
        return (TRParen, ExplicitParameter)
      TLBrace -> do
        match TLBrace
        return (TRBrace, ImplicitParameter)
      _ -> failM "Expected a parenthesized parameter."
  let mTerminator = if allowBareParameters then [terminator] else []
  names <- parseSequenceUntil ([TColon] ++ mTerminator) parseParameterName
  tokTyp' <- peekType 
  typs <- if tokTyp' == TColon
           then do match TColon
                   expr <- parseExpr
                   return [expr | _ <- names]
           else return [EWildcard pos | _ <- names]
  match terminator
  return $ zipWith (\ name typ -> paramConstructor pos name typ) names typs

----

parseDelimitedSequence :: TokenType -> TokenType -> TokenType -> M a -> M [a]
parseDelimitedSequence tBegin tSep tEnd parseElem =
  parseDelimitedSequenceL tBegin tSep tEnd
                          (do elem <- parseElem ; return [elem])

parseDelimitedSequenceL :: TokenType -> TokenType -> TokenType -> M [a] -> M [a]
parseDelimitedSequenceL tBegin tSep tEnd parsePart = do
    match tBegin
    tokTyp <- peekType
    res <- if tokTyp == tEnd
            then return []
            else parseSeq1
    match tEnd
    return res
  where
    parseSeq1 = do
      elems1 <- parsePart
      tokTyp <- peekType
      if tokTyp == tSep
       then do match tSep
               elems2 <- parseSeq1
               return (elems1 ++ elems2)
       else return elems1

parseSequenceUntil :: [TokenType] -> M a -> M [a]
parseSequenceUntil terminators parseElem = do
  tokTyp <- peekType
  if tokTyp `elem` terminators
   then do return []
   else do x <- parseElem
           xs <- parseSequenceUntil terminators parseElem
           return (x : xs)

----

parseLambda :: M Expr
parseLambda = do
  pos <- getPos
  match TLambda
  tokTyps <- peek2Types
  case tokTyps of
    (TLBrace, TRBrace) -> parseLambdaCase pos
    _ -> do
      mParameters <- tryOrBacktrackFS
                       (do parameters <- parseParenthesizedTypeParameters True
                           return (Just parameters))
                       (return Nothing)
      case mParameters of
        Just parameters -> do
          -- Lambda with bare parameters
          match (TId arrowOperator)
          body <- parseExpr
          return $ lamManyParams parameters body
        Nothing -> parseLambdaCase pos
  where
    parseLambdaCase :: Pos.Position -> M Expr
    parseLambdaCase pos = do
      -- Lambda as a case branch
      branches <- parseCaseBranches
      var <- freshVarName
      return $ ELam pos var (EWildcard pos) (ECase pos (EVar pos var) branches)

parseCase :: M Expr
parseCase = do
  pos <- getPos
  match TCase
  guard <- parseExpr
  match TOf
  branches <- parseCaseBranches
  return $ ECase pos guard branches

parseCaseBranches :: M [CaseBranch]
parseCaseBranches = parseDelimitedSequence TLBrace TSemicolon TRBrace
                                           parseCaseBranch

parseCaseBranch :: M CaseBranch
parseCaseBranch = do
  pos <- getPos
  patterns <- parseSequenceUntil [TId arrowOperator] parsePattern
  match (TId arrowOperator)
  body <- parseExpr
  return $ CaseBranch pos patterns body

parsePattern :: M Pattern
parsePattern = do
  atExpr <- parseAtomicExpr
  case atExpr of
    OptOptional _ -> failM ("Pattern cannot be in braces.")
    OptPlain expr ->
      case exprToPattern expr of
        Nothing      -> failM ("Expected a pattern. Found: " ++ show expr)
        Just pattern -> return pattern
    
----

getPos :: M Pos.Position
getPos = do
  state <- getFS
  case next state of
    (tok : _) -> return $ startPos tok
    []        ->
      case prev state of
        (tok : _) -> return $ startPos tok
        []        -> return $ Pos.unknown

match :: TokenType -> M ()
match tokTyp = do
  tokTyp' <- peekType
  if tokTyp == tokTyp'
   then nextToken
   else failM $ unlines [
                  "Expected: " ++ show tokTyp
                , "But got : " ++ show tokTyp'
                ]

peek :: M Token
peek = do
  state <- getFS
  case next state of
    (tok : _) -> return tok
    [] -> do
      pos <- getPos
      return $ Token pos TEof

peek2 :: M (Token, Token)
peek2 = do
  tok1 <- peek
  oldState <- getFS
  nextToken
  tok2 <- peek
  putFS oldState
  return (tok1, tok2)

nextToken :: M ()
nextToken = do
  state <- getFS
  putFS (state {
           prev = head (next state) : prev state
         , next = tail (next state)
         })

peekType :: M TokenType
peekType = tokenType <$> peek

peek2Types :: M (TokenType, TokenType)
peek2Types = do
  (t1, t2) <- peek2
  return (tokenType t1, tokenType t2)

failM :: String -> M a
failM msg = do
  pos <- getPos
  failFS $ unlines [
             "Near: " ++ show pos
           , "Parse error"
           , msg
           ]

perform :: Either String a -> M a
perform (Left msg) = failM msg
perform (Right x)  = return x

---- Modules ----

enterModule :: ModuleName -> MS.Exports -> M ()
enterModule suffix exports = do
  prefix <- currentModuleName <$> getFS
  let fullModuleName = makeModuleName (moduleNameParts prefix ++
                                       moduleNameParts suffix)
  -- Define module.
  ms <- moduleSystem <$> getFS
  moduleSystem' <- perform $ MS.addModule ms fullModuleName exports
  modifyFS (\ state -> state { moduleSystem = moduleSystem' })
  -- Set this as the current module.
  modifyFS (\ state -> state { currentModuleName = fullModuleName })

exitModule :: ModuleName -> M ()
exitModule suffix = do
  fullModuleName <- currentModuleName <$> getFS
  let prefix = makeModuleName $
                 dropLast (length (moduleNameParts suffix))
                          (moduleNameParts fullModuleName)
  modifyFS (\ state -> state { currentModuleName = prefix })

moduleImport :: ModuleName -> MS.Imports -> M ()
moduleImport importedModuleName imports = do
  ms  <- moduleSystem <$> getFS
  mod <- currentModuleName <$> getFS
  moduleSystem' <- perform $ MS.moduleImport ms mod importedModuleName imports
  modifyFS (\ state -> state { moduleSystem = moduleSystem' })

registerLocalName :: BareName -> M QName
registerLocalName bareName = do
    ms  <- moduleSystem  <$> getFS
    mod <- currentModuleName <$> getFS
    ms' <- perform $ MS.registerLocalName ms mod bareName
    modifyFS (\ state -> state { moduleSystem = ms' })
    qname <- resolveNameOrFail (makeQName rootModuleName bareName)
    -- Register as operator if not registered
    pt <- precedenceTable <$> getFS
    if qnameIsOperator qname && not (PT.isOperatorName pt qname)
     then declareOperator defaultAssociativity defaultPrecedence qname
     else return ()
    return qname
  where
    resolveNameOrFail :: QName -> M QName
    resolveNameOrFail qname = resolveNameOrDo failM qname
    defaultAssociativity :: PT.Associativity
    defaultAssociativity = PT.NonAssoc
    defaultPrecedence :: PT.Precedence
    defaultPrecedence = PT.Precedence 20

registerLocalQName :: QName -> M ()
registerLocalQName qname = do
  let bareName = qnameBareName qname
  qname' <- registerLocalName bareName
  if qname /= qname'
    then failM ("Conflict when registering name: " ++ show qname)
    else return ()

resolveNameOrDo :: (ErrMsg -> M QName) -> QName -> M QName
resolveNameOrDo handler qname = do
  ms  <- moduleSystem  <$> getFS
  mod <- currentModuleName <$> getFS
  case MS.resolveName ms mod qname of
    Left msg -> if isBareQName qname
                 then handler msg
                 else failM msg
    Right qname' -> return qname'

resolveNameOrRegister :: QName -> M QName
resolveNameOrRegister qname = do
  resolveNameOrDo (\ msg -> registerLocalName (qnameBareName qname))
                  qname

resolveNameOrReturnLocalName :: QName -> M QName
resolveNameOrReturnLocalName qname =
  resolveNameOrDo
    (\ msg ->
      if isBareQName qname
       then do mod <- currentModuleName <$> getFS
               return $ makeQName mod (qnameBareName qname)
       else failM msg)
    qname

---- Precedence table ----

declareOperator :: PT.Associativity -> PT.Precedence -> QName -> M ()
declareOperator associativity precedence qName = do
  pt <- precedenceTable <$> getFS
  pt' <- perform $ PT.declareOperator pt associativity precedence qName
  modifyFS (\ state -> state { precedenceTable = pt' })

----

freshVarName :: M QName
freshVarName = do
  pos <- getPos
  n <- nextFreshVar <$> getFS
  modifyFS (\ state -> state {
              nextFreshVar = nextFreshVar state + 1
           })
  return $ makeInternalQName "p" n


isTId :: TokenType -> Bool
isTId (TId _) = True
isTId _       = False

isTUnderscore :: TokenType -> Bool
isTUnderscore TUnderscore = True
isTUnderscore _           = False

isTParamName :: TokenType -> Bool
isTParamName typ = (isTId typ && tIdQName typ `S.notMember` reservedWords)
                || isTUnderscore typ
  where
    tIdQName (TId qname) = qname
    tIdQName _           = error "(Impossible)"

