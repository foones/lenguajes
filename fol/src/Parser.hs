module Parser(tokenize, parse) where

import Expr(
         Id(..),
         HypId(..), TyconId(..), TyvarId(..), PredId(..), FunId(..), VarId(..),
         Declaration(..),
         Metavar(..), firstMetavar, nextMetavar,
         Type(..), Term(..), Form(..),
         ConstructorDecl(..), DefEquation(..), Proof(..), OptionalName(..),
         HoleName(..), OptionalForm(..),
         ThusType(..),
         Command(..), Justification(..),
         CaseBranch(..), IndBranch(..), IndPattern(..),
         freeVars
       )

import FailState(FailState, getFS, putFS, modifyFS, failFS, runFS)
import qualified Data.Set as S
import qualified Data.Map as M

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isSymbol :: Char -> Bool
isSymbol c = c `elem` "_!#$%&*+-./<=>?@'"

isIdent :: Char -> Bool
isIdent c = isAlpha c || isDigit c || isSymbol c

----

data Token = T_EOF
           | T_Id Id
           | T_Hole HoleName
           | T_LParen
           | T_RParen
           | T_Colon
           | T_Comma
           | T_Underscore
           | T_Eq
           -- Formulae
           | T_True
           | T_False
           | T_And
           | T_Or
           | T_Imp
           | T_Iff
           | T_Neg
           | T_Forall
           | T_Exists
           -- Declarations
           | T_Data
           | T_Prop
           | T_Axiom
           | T_Theorem
           | T_Def
           | T_End
           -- Proofs
           | T_Admit
           | T_By
           | T_Contradiction
           | T_Have    | T_Then
           | T_Suppose
           | T_Thus    | T_Hence  | T_Conclude
           | T_Let
           | T_Cases
           | T_Case
           | T_Take
           | T_Claim
           | T_Consider
           | T_St
           | T_Induction
           | T_Show
           -- Tokens representing sequences of two tokens that are merged
           -- in an intermediate pass.
           | T_ThenCases
           | T_ThenHave
           | T_ThenConclude
           | T_ThenContradiction
           | T_ThenConsider
  deriving (Eq, Ord, Show)

declarationTokens :: S.Set Token
declarationTokens =
  S.fromList [
    T_Data, T_Prop, T_Axiom, T_Theorem, T_Def
  ]

commandTokens :: S.Set Token
commandTokens = 
  S.fromList [
    T_Admit, T_By, T_Contradiction, T_End, T_Have, T_Then, T_Suppose, T_Thus,
    T_Hence, T_Conclude, T_Let, T_Cases, T_Case, T_Take,
    T_Claim, T_Consider, T_St, T_Induction, T_Show,
    T_ThenCases, T_ThenHave, T_ThenConclude, T_ThenContradiction, T_ThenConsider
  ]

keywords :: M.Map String Token
keywords = M.fromList [
             ("_", T_Underscore)
           -- Formulae
           , ("forall", T_Forall)
           , ("exists", T_Exists)
           , ("true", T_True)
           , ("false", T_False)
           -- Declarationsj
           , ("axiom", T_Axiom)
           , ("data", T_Data)
           , ("def", T_Def)
           , ("theorem", T_Theorem)
           , ("prop", T_Prop)
           , ("end", T_End)
           -- Proofs
           , ("admit", T_Admit)
           , ("by", T_By)
           , ("contradiction", T_Contradiction)
           , ("have", T_Have)
           , ("then", T_Then)
           , ("thus", T_Thus)
           , ("hence", T_Hence)
           , ("conclude", T_Conclude)
           , ("suppose", T_Suppose)
           , ("let", T_Let)
           , ("cases", T_Cases)
           , ("case", T_Case)
           , ("take", T_Take)
           , ("claim", T_Claim)
           , ("consider", T_Consider)
           , ("st", T_St)
           , ("induction", T_Induction)
           , ("show", T_Show)
           ]

tokenize :: String -> [Token]
tokenize = join . tok
  where
    tok :: String -> [Token]
    tok ""               = []
    tok (' ' : cs)       = tok cs
    tok ('\n' : cs)      = tok cs
    tok ('\r' : cs)      = tok cs
    tok ('\t' : cs)      = tok cs
    tok ('-' : '-' : cs) = let (_, cs') = span (/= '\n') cs in
                                  tok cs'
    tok ('{' : '-' : cs) = tok (rec 1 cs)
      where
        rec n ('{' : '-' : cs) = rec (n + 1) cs
        rec n ('-' : '}' : cs) = if n == 1
                                  then cs
                                  else rec (n - 1) cs
        rec n (_ : cs)        = rec n cs
        rec _ []              = error "Unclosed multiline comment."
    tok ('.' : '.' : '.' : cs)
                              = T_Admit : tok cs
    tok ('=' : cs)       = T_Eq : tok cs
    tok (',' : cs)       = T_Comma : tok cs
    tok (':' : cs)       = T_Colon : tok cs
    tok ('(' : cs)       = T_LParen : tok cs
    tok (')' : cs)       = T_RParen : tok cs
    tok ('&' : cs)       = T_And : tok cs
    tok ('∧' : cs)       = T_And : tok cs
    tok ('|' : cs)       = T_Or : tok cs
    tok ('∨' : cs)       = T_Or : tok cs
    tok ('<' : '-' : '>' : cs)
                              = T_Iff : tok cs
    tok ('-' : '>' : cs) = T_Imp : tok cs
    tok ('→' : cs)       = T_Imp : tok cs
    tok ('¬' : cs)       = T_Neg : tok cs
    tok ('∀' : cs)       = T_Forall : tok cs
    tok ('∃' : cs)       = T_Exists : tok cs
    tok cs@(c : _)
      | isIdent c =
        let (id, cs') = span isIdent cs in
          case M.lookup id keywords of
            Just token -> token : tok cs'
            _ ->
              let tId = if head id == '?'
                         then T_Hole (tail id)
                         else T_Id (Id id)
               in tId : tok cs'
    tok cs = error ("Invalid input:\n" ++ take 10 cs)
    join :: [Token] -> [Token]
    join [] = []
    join (T_Then : T_Cases : toks)         = T_ThenCases    : join toks
    join (T_Then : T_Have : toks)          = T_ThenHave     : join toks
    join (T_Then : T_Conclude : toks)      = T_ThenConclude : join toks
    join (T_Then : T_Contradiction : toks) = T_ThenContradiction : join toks
    join (T_Then : T_Consider : toks)      = T_ThenConsider : join toks
    join (tok : toks)                      = tok            : join toks

----

data SymbolType = FunctionSymbol | PredicateSymbol | TypeConstructor
  deriving Eq

data LocalVariable = LVar | LEigenVar

instance Show SymbolType where
  show FunctionSymbol  = "function symbol"
  show PredicateSymbol = "predicate symbol"
  show TypeConstructor = "type constructor"

data ParserState = PS {
                     psSymbols     :: M.Map Id SymbolType
                   , psEnvironment :: [M.Map VarId LocalVariable]
                   , psInput       :: [Token]
                   }

type Parser a = FailState String ParserState a

---- Monad

registerSymbol :: SymbolType -> Id -> Parser ()
registerSymbol typ name = do
  state <- getFS
  case M.lookup name (psSymbols state) of
    Nothing ->
      modifyFS $ \ state -> state {
                   psSymbols = M.insert name typ (psSymbols state)
                 }
    Just typ' ->
      failFS ("Symbol " ++ show name ++ " declared as " ++ show typ
              ++ " has already been declared as " ++ show typ' ++ ".")

isRegisteredSymbol :: SymbolType -> Id -> Parser Bool
isRegisteredSymbol typ name = do
  state <- getFS
  case M.lookup name (psSymbols state) of
    Just typ' | typ == typ' -> return True
    _ -> return False

isFunctionSymbol :: Id -> Parser Bool
isFunctionSymbol = isRegisteredSymbol FunctionSymbol

isPredicateSymbol :: Id -> Parser Bool
isPredicateSymbol = isRegisteredSymbol PredicateSymbol

isTypeConstructor :: Id -> Parser Bool
isTypeConstructor = isRegisteredSymbol TypeConstructor

pushScope :: Parser ()
pushScope = 
  modifyFS $ \ state -> state {
               psEnvironment = M.empty : psEnvironment state
             }

popScope :: Parser ()
popScope = 
  modifyFS $ \ state -> state {
               psEnvironment = tail (psEnvironment state)
             }

bindLocalVariable :: LocalVariable -> VarId -> Parser ()
bindLocalVariable localVar name = do
  modifyFS $ \ state -> state {
               psEnvironment =
                   M.insert name localVar (head (psEnvironment state))
                 : tail (psEnvironment state)
             }

isEigenVariable :: VarId -> Parser Bool
isEigenVariable x = do
    env <- psEnvironment <$> getFS
    return $ rec env
  where
    rec [] = False
    rec (rib : env) =
      case M.lookup x rib of
        Just LEigenVar -> True
        Just _         -> False
        Nothing        -> rec env

peek :: Parser Token
peek = do
  toks <- psInput <$> getFS
  case toks of
    []        -> return T_EOF
    (tok : _) -> return tok

peek2 :: Parser (Token, Token)
peek2 = do
  toks <- psInput <$> getFS
  case toks of
    []                -> return (T_EOF, T_EOF)
    [tok1]            -> return (tok1, T_EOF)
    (tok1 : tok2 : _) -> return (tok1, tok2)

nextTok :: Parser ()
nextTok = do
  state <- getFS
  putFS $ state { psInput = tail (psInput state) }

matchAny :: [Token] -> Parser Token
matchAny expectedToks = do
  tok <- peek
  if tok `elem` expectedToks
   then do nextTok
           return tok
   else failFS ("Expected: " ++ show expectedToks ++ " but got: " ++ show tok)

match :: Token -> Parser ()
match expectedTok = do
  matchAny [expectedTok]
  return ()

----

parse :: [Token] -> [Declaration]
parse t0 =
  case runFS parseDeclarations initialState of
    Left msg -> error msg
    Right (decls, state) -> 
      case psInput state of
        [] -> decls
        _  -> error "Trailing input."
  where
    initialState :: ParserState
    initialState = PS {
                     psSymbols     = M.empty
                   , psEnvironment = []
                   , psInput       = t0
                   }

parseId :: Parser Id
parseId = do
  tok <- peek
  case tok of
    T_Id id -> do nextTok
                  return id
    _ -> failFS "Expected an identifier."

parseDeclarations :: Parser [Declaration]
parseDeclarations = do
  tok <- peek
  case tok of
    T_EOF -> return []
    _     -> do decl  <- parseDeclaration
                decls <- parseDeclarations
                return (decl : decls)

parseDeclaration :: Parser Declaration
parseDeclaration = do
  tok <- peek
  case tok of
    T_Data    -> parseDataDeclaration
    T_Prop    -> parsePropDeclaration
    T_Axiom   -> parseAxiom
    T_Theorem -> parseTheorem
    T_Def     -> parseDef
    _ -> failFS "Expected a declaration."

parseDataDeclaration :: Parser Declaration
parseDataDeclaration = do
  match T_Data
  tok <- peek
  case tok of
    T_Id x -> do
      registerSymbol TypeConstructor x
      typ <- parseType
      tok2 <- peek
      case tok2 of
        T_Eq -> do match T_Eq
                   cs <- parseConstructorDeclarations
                   return $ DeclData typ cs
        _ -> return $ DeclData typ []
    _ -> failFS "Expected a type."

parseTypeList :: Parser [Type]
parseTypeList = do
  tok <- peek
  case tok of
    T_LParen -> do
      match T_LParen
      typs <- parseTypes
      match T_RParen
      return typs
    _ -> return []

parsePropDeclaration :: Parser Declaration
parsePropDeclaration = do
  match T_Prop
  tok <- peek
  case tok of
    T_Id x -> do
      registerSymbol PredicateSymbol x
      nextTok
      typs <- parseTypeList
      return $ DeclProp (PredId x) typs
    _ -> failFS "Expected a prop declaration."

parseConstructorDeclarations :: Parser [ConstructorDecl]
parseConstructorDeclarations =
    parseSequence1 "constructor declaration"
                   startsConstructor T_Or parseConstructorDeclaration 
  where
    startsConstructor (T_Id _) = True
    startsConstructor _        = False

parseConstructorDeclaration :: Parser ConstructorDecl
parseConstructorDeclaration = do
  tok <- peek
  case tok of
    T_Id x -> do
      registerSymbol FunctionSymbol x
      nextTok
      tok2 <- peek
      case tok2 of
        T_LParen -> do match T_LParen
                       typs <- parseTypes
                       match T_RParen
                       return $ ConstructorDecl (FunId x) typs
        _ -> return $ ConstructorDecl (FunId x) []
    _ -> failFS "Expected a constructor declaration."

parseType :: Parser Type
parseType = do
  tok <- peek
  case tok of
    T_Id x -> do
      nextTok
      bIsTypeConstructor <- isTypeConstructor x
      if bIsTypeConstructor
       then do
         tok2 <- peek
         if tok2 == T_LParen
          then do match T_LParen
                  typs <- parseTypes
                  match T_RParen
                  return $ TyApp (TyconId x) typs
          else return $ TyApp (TyconId x) []
       else do
         tok2 <- peek
         if tok2 == T_LParen
          then failFS (
                  "Type variable \"" ++ show x ++ "\" is applied.\n"
               ++ "Perhaps you forgot to declare the type constructor?"
               )
          else do
            return $ TyVar (TyvarId x)
    _ -> failFS "Expected a type."

universalClosure :: Form -> Parser Form
universalClosure form = do
  let fvs = S.toList (freeVars form)
  return $ foldr (uncurry FForall) form (zip fvs (repeat TyUnknown))

parseAxiom :: Parser Declaration
parseAxiom = do
  match T_Axiom
  name <- parseId
  match T_Colon
  form <- parseForm
  form' <- universalClosure form
  return $ DeclAxiom (HypId name) form'

parseTheorem :: Parser Declaration
parseTheorem = do
  match T_Theorem
  name <- parseId
  match T_Colon
  form <- parseForm
  pushScope
  proof <- parseProof [T_End]
  popScope
  match T_End
  form' <- universalClosure form
  return $ DeclTheorem (HypId name) form' proof

parseDef :: Parser Declaration
parseDef = do
  match T_Def
  name <- parseId
  registerSymbol FunctionSymbol name
  argTyps <- parseTypeList
  resTyp  <- parseOptionalType
  equations <- parseDefEquations name
  return $ DeclDef (FunId name) argTyps resTyp equations

parseDefEquations :: Id -> Parser [DefEquation]
parseDefEquations name = do
    tok <- peek
    if tok `S.member` nextToks
     then return []
     else do
       eq  <- parseDefEquation
       eqs <- parseDefEquations name
       return (eq : eqs)
  where
    nextToks :: S.Set Token
    nextToks = S.insert T_EOF declarationTokens
    parseDefEquation :: Parser DefEquation
    parseDefEquation = do
      lhs <- parseTerm
      case lhs of
        TApp f lhsArgs | f == FunId name -> do
          match T_Eq
          rhs <- parseTerm
          return (DefEquation lhsArgs rhs)
        _ -> failFS ("Expected function symbol '" ++ show name ++ "'"
                    ++ " on the left-hand side of the equation.")

parseForm :: Parser Form
parseForm = parseBinOp T_Iff (\ a b -> FAnd (FImp a b) (FImp b a))
          . parseBinOp T_Imp FImp
          . parseBinOp T_Or FOr
          . parseBinOp T_And FAnd
          $ parseFormAtom

parseFormAtom :: Parser Form
parseFormAtom = do
  tok <- peek
  case tok of
    T_Id p -> do
      bIsPredicateSymbol <- isPredicateSymbol p
      if bIsPredicateSymbol
       then do
         nextTok
         tok2 <- peek
         if tok2 == T_LParen
          then do match T_LParen
                  terms <- parseTerms
                  match T_RParen
                  return $ FPred (PredId p) terms
          else return $ FPred (PredId p) []
       else failFS ("Atomic formula \"" ++ show p ++ "\" is not a predicate symbol.")
    T_True   -> do match T_True
                   return $ FTrue
    T_False  -> do match T_False
                   return $ FFalse
    T_Forall -> parseQuantifier T_Forall FForall
    T_Exists -> parseQuantifier T_Exists FExists
    T_Neg -> do
      match T_Neg
      form <- parseFormAtom
      return $ FNot form
    T_LParen -> do
      match T_LParen
      form <- parseForm
      match T_RParen
      return form
    _ -> failFS "Expected an atomic formula."

parseQuantifier :: Token -> (VarId -> Type -> Form -> Form) -> Parser Form
parseQuantifier qToken quantifier = do
  match qToken
  typedVarIds <- parseTypedVarIds
  match T_Comma
  pushScope
  mapM_ (bindLocalVariable LVar) (map fst typedVarIds)
  body <- parseForm
  popScope
  return $ foldr (uncurry quantifier) body typedVarIds

parseOptionalType :: Parser Type
parseOptionalType = do
  tok <- peek 
  if tok == T_Colon
   then do match T_Colon
           parseType
   else return TyUnknown

parseBinOp :: Token -> (Form -> Form -> Form) -> Parser Form -> Parser Form
parseBinOp tokOp formOp parseInner = do
  form <- parseInner
  tok <- peek
  if tok == tokOp
   then do match tok
           form' <- parseBinOp tokOp formOp parseInner
           return $ formOp form form'
   else return form

parseIds :: Parser [Id]
parseIds = do
  tok <- peek
  case tok of 
    T_Id x -> do
      nextTok
      xs <- parseIds
      return (x : xs)
    _ -> return []

parseTypedVarIds :: Parser [(VarId, Type)]
parseTypedVarIds = do
    typedIds <- rec
    ty <- parseOptionalType
    return $ map (\ (x, tyX) ->
                   if tyX == TyUnknown
                    then (x, ty)
                    else (x, tyX))
                 typedIds
  where
    rec :: Parser [(VarId, Type)]
    rec = do
      tok <- peek
      case tok of 
        T_Id x -> do
          nextTok
          xs <- rec
          return ((VarId x, TyUnknown) : xs)
        T_LParen -> do
          match T_LParen
          xs <- parseIds
          match T_Colon
          ty <- parseType
          match T_RParen
          ys <- rec
          return ([(VarId x, ty) | x <- xs] ++ ys)
        _ -> return []

parseTerm :: Parser Term
parseTerm = do
  tok <- peek
  case tok of
    T_Id x -> do
      nextTok
      bIsFunctionSymbol <- isFunctionSymbol x
      if bIsFunctionSymbol
       then do
         tok2 <- peek
         if tok2 == T_LParen
          then do match T_LParen
                  terms <- parseTerms
                  match T_RParen
                  return $ TApp (FunId x) terms
          else return $ TApp (FunId x) []
       else do
         tok2 <- peek
         if tok2 == T_LParen
          then failFS (
                 "Variable \"" ++ show x ++ "\" is applied.\n"
               ++ "Perhaps you forgot to declare it as a function symbol?"
               )
          else do
            be <- isEigenVariable (VarId x)
            if be
             then return $ TEigenVar (VarId x)
             else return $ TVar (VarId x)
    _ -> failFS "Expected a term."

parseSequence :: String -> (Token -> Bool) -> Token -> Parser a -> Parser [a]
parseSequence itemName startsItem delim parseItem = do
  tok <- peek
  if startsItem tok
   then parseSequence1 itemName startsItem delim parseItem
   else return []

parseSequence1 :: String -> (Token -> Bool) -> Token -> Parser a -> Parser [a]
parseSequence1 itemName startsItem delim parseItem = do
  tok <- peek
  if tok /= T_EOF && startsItem tok
   then do
     item <- parseItem
     tok2 <- peek
     if tok2 == delim
      then do match delim
              items <- parseSequence1 itemName startsItem delim parseItem
              return (item : items)
      else return [item]
   else failFS ("Expected a " ++ itemName ++ ".")

parseTerms :: Parser [Term]
parseTerms = parseSequence "term" startsTerm T_Comma parseTerm
  where
    startsTerm (T_Id _) = True
    startsTerm _        = False

parseTypes :: Parser [Type]
parseTypes = parseSequence "type" startsType T_Comma parseType
  where
    startsType (T_Id _) = True
    startsType _        = False

parseProof :: [Token] -> Parser Proof
parseProof terminators = do
  tok <- peek
  if tok `elem` terminators
   then return ProofEmpty
   else do cmd   <- parseCommand
           proof <- parseProof terminators
           return $ ProofCons cmd proof

parseOName :: Parser OptionalName
parseOName = do
  (tok1, tok2) <- peek2
  case (tok1, tok2) of
    (T_Id name, T_Colon) -> do
      nextTok
      match T_Colon
      return $ ONName (HypId name)
    _ -> return ONNone

parseOForm :: Parser OptionalForm
parseOForm = do
  tok <- peek
  case tok of
    T_Underscore -> do
      match T_Underscore
      return OFNone
    T_Hole holeName -> do
      nextTok
      return $ OFHole holeName
    _ -> do
      form <- parseForm
      return $ OFForm form

parseOptionalOrEmptyFormula :: Parser OptionalForm
parseOptionalOrEmptyFormula = do
    tok <- peek
    if tok `S.member` commandTokens
     then return OFNone
     else parseOForm

parseOptionalJustifications :: Parser [Justification]
parseOptionalJustifications = do
  tok <- peek
  case tok of
    T_By -> do
      match T_By
      parseJustificationList1
    _ -> return []

parseJustification :: Parser Justification
parseJustification = do
  tok <- peek
  case tok of
    T_Underscore -> do
      match T_Underscore
      return JPreviousHypothesis
    _ -> JHypothesis . HypId <$> parseId

-- Comma-separated, non-empty list of justifications.
parseJustificationList1 :: Parser [Justification]
parseJustificationList1 = do
  j <- parseJustification
  tok <- peek
  if tok == T_Comma
   then do match T_Comma
           js <- parseJustificationList1
           return (j : js)
   else return [j]

parseCommand :: Parser Command
parseCommand = do
  tok <- peek
  case tok of
    T_Suppose -> do
      match T_Suppose
      oName <- parseOName
      oForm <- parseOptionalOrEmptyFormula
      return $ CmdSuppose oName oForm
    t_Thus | t_Thus `elem` [T_Thus, T_Hence, T_Admit,
                            T_Conclude, T_Contradiction,
                            T_ThenConclude, T_ThenContradiction] -> do
      nextTok
      oName <- parseOName
      oForm <- parseOptionalOrEmptyFormula
      oJustifications <- parseOptionalJustifications
      let oJustifications' =
            (if t_Thus `elem` [T_Hence, T_ThenConclude, T_ThenContradiction]
              then [JPreviousHypothesis]
              else [])
            ++ oJustifications
      let thusType =
            case t_Thus of
              T_Conclude          -> C_Thus -- conclude/thus/hence are synonyms
              T_Thus              -> C_Thus
              T_Hence             -> C_Thus
              T_ThenConclude      -> C_Thus
              T_Admit             -> C_Admit
              T_Contradiction     -> C_Contradiction
              T_ThenContradiction -> C_Contradiction
              _ -> error "(Impossible)"
      return $ CmdThus thusType oName oForm oJustifications'
    t_Have | t_Have `elem` [T_Have, T_Then, T_ThenHave] -> do
      nextTok
      oName <- parseOName
      form <- parseForm
      oJustifications <- parseOptionalJustifications
      let oJustifications' = (if t_Have `elem` [T_Then, T_ThenHave]
                               then [JPreviousHypothesis]
                               else [])
                             ++ oJustifications
      return $ CmdHave oName form oJustifications'
    T_Let -> do
      match T_Let
      typedVarIds <- parseTypedVarIds
      mapM_ (bindLocalVariable LEigenVar) (map fst typedVarIds)
      return $ CmdLet typedVarIds
    t_Cases | t_Cases `elem` [T_Cases, T_ThenCases] -> parseCases
    T_Take -> do
      match T_Take
      term <- parseTerm
      return $ CmdTake term
    t_Consider | t_Consider `elem` [T_Consider, T_ThenConsider] -> do
      t_Consider <- matchAny [T_Consider, T_ThenConsider]
      typedVarIds <- parseTypedVarIds
      match T_St
      oName <- parseOName
      pushScope
      mapM_ (bindLocalVariable LVar) (map fst typedVarIds)
      form <- parseForm
      popScope
      oJustifications <- parseOptionalJustifications
      let oJustifications' =
            (if t_Consider `elem` [T_ThenConsider]
              then [JPreviousHypothesis]
              else [])
            ++ oJustifications
      mapM_ (bindLocalVariable LEigenVar) (map fst typedVarIds)
      return $ CmdConsider typedVarIds oName form oJustifications'
    T_Claim -> do
      match T_Claim
      pushScope
      oName <- parseOName
      form  <- parseForm
      proof <- parseProof [T_End]
      popScope
      match T_End
      return $ CmdClaim oName form proof
    T_Induction -> parseInduction
    T_Show -> do
      match T_Show
      form <- parseForm
      return $ CmdShow form
    _ -> failFS "Expected a command"

parseCases :: Parser Command
parseCases = do
    t_Cases <- matchAny [T_Cases, T_ThenCases]
    oForm <- parseOptionalOrEmptyFormula
    oJustifications <- parseOptionalJustifications
    let oJustifications' =
          (if t_Cases `elem` [T_ThenCases]
            then [JPreviousHypothesis]
            else [])
          ++ oJustifications
    branches <- parseCaseBranches
    match T_End
    return $ CmdCases oForm oJustifications' branches
  where
    parseCaseBranches :: Parser [CaseBranch]
    parseCaseBranches = do
      tok <- peek
      case tok of
        T_Case -> do
          match T_Case
          pushScope
          oName <- parseOName
          form <- parseForm
          proof <- parseProof [T_Case, T_End]
          popScope
          branches <- parseCaseBranches
          return (CaseBranch oName form proof : branches)
        _ -> return []

parseTypedVarIdList :: Parser [(VarId, Type)]
parseTypedVarIdList = do
  tok <- peek
  case tok of
    T_LParen -> do
      match T_LParen
      tok2 <- peek 
      if tok2 == T_RParen
       then do match T_RParen
               return []
       else do typedVarIds <- parseTypedVarIdList1 
               match T_RParen
               return typedVarIds
    _ -> return []

parseTypedVarIdList1 :: Parser [(VarId, Type)]
parseTypedVarIdList1 = do
  id <- VarId <$> parseId
  ty <- parseOptionalType
  tok <- peek
  if tok == T_Comma
   then do match T_Comma
           typedIds <- parseTypedVarIdList1
           return ((id, ty) : typedIds)
   else return [(id, ty)]

parseInduction :: Parser Command
parseInduction = do
    match T_Induction
    tok <- peek 
    typ <- if tok `elem` [T_Case, T_End]
            then return TyUnknown
            else parseType
    branches <- parseIndBranches
    match T_End
    return $ CmdInduction typ branches
  where
    parseIndBranches :: Parser [IndBranch]
    parseIndBranches = do
      tok <- peek
      case tok of
        T_Case -> do
          match T_Case
          pushScope
          pattern@(IndPattern _ typedIds) <- parseIndPattern
          mapM_ (bindLocalVariable LEigenVar) (map fst typedIds)
          proof <- parseProof [T_Case, T_End]
          popScope
          branches <- parseIndBranches
          return (IndBranch pattern proof : branches)
        _ -> return []
    parseIndPattern :: Parser IndPattern
    parseIndPattern = do
      constructor <- parseId
      typedVarIds <- parseTypedVarIdList
      return $ IndPattern (FunId constructor) typedVarIds

