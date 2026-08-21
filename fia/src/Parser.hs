module Parser(parse) where

import qualified Data.Set as S
import qualified Data.Map as M

import FailState(FailState, evalFS, getFS, modifyFS, putFS, failFS, logFS)
import Token(Token(..), isLowerName, isUpperName)
import Syntax(
         HoleId(..), HypId(..), NatVarId(..), FunId(..), PropId(..), FormVarId(..),
         NatVarKind(..),
         Program(..), Declaration(..), Nat(..), Form(..), Proof(..),
         CaseBranch(..), EqChain(..), PropParam(..), PropArg(..),
         proofLet, proofApply
       )
import Pprint(pprintPropParams, pprintPropArgs, pprintForm, pprintNat)

length' :: [a] -> Integer
length' xs = fromIntegral (length xs)

data ParserState = ParserState {
                     sTokens          :: [Token]
                   , sHypotheses      :: S.Set String
                   , sNatVariables    :: M.Map String NatVarKind
                   , sFormVariables   :: S.Set String
                   , sFunctionSymbols :: M.Map String Integer
                   , sPropSymbols     :: M.Map String [PropParam]
                   }

type M = FailState ParserState

----

failM :: String -> M a
failM msg = do
  toks <- sTokens <$> getFS
  failFS (unlines [
       "!!! Parse error"
     , msg
     , "Near:"
     , "    " ++ show (take 10 toks)
     ])

----

nameIsHypothesisM :: String -> M Bool
nameIsHypothesisM name = do
  state <- getFS
  return (name `S.member` sHypotheses state)

nameIsNatVariableM :: String -> M Bool
nameIsNatVariableM name = do
  state <- getFS
  return (name `M.member` sNatVariables state)

nameIsFormVariableM :: String -> M Bool
nameIsFormVariableM name = do
  state <- getFS
  return (name `S.member` sFormVariables state)

nameIsFunctionSymbolM :: String -> M Bool
nameIsFunctionSymbolM name = do
  state <- getFS
  return (name `M.member` sFunctionSymbols state)

getFunctionSymbolArityM :: String -> M Integer
getFunctionSymbolArityM name = do
  state <- getFS
  return (M.findWithDefault (error "impossible") name (sFunctionSymbols state))

nameIsPropSymbolM :: String -> M Bool
nameIsPropSymbolM name = do
  state <- getFS
  return (name `M.member` sPropSymbols state)

nameIsUnboundM :: String -> M Bool
nameIsUnboundM name = do
  m1 <- not <$> nameIsHypothesisM name
  m2 <- not <$> nameIsNatVariableM name
  m3 <- not <$> nameIsFormVariableM name
  m4 <- not <$> nameIsFunctionSymbolM name
  m5 <- not <$> nameIsPropSymbolM name
  return (m1 && m2 && m3 && m4 && m5)

tokenIsId :: Token -> Bool
tokenIsId (TId _) = True
tokenIsId _       = False

checkIsUnboundM :: String -> M ()
checkIsUnboundM hypName = do
  b <- nameIsUnboundM hypName
  if b
   then return ()
   else failM ("\"" ++ hypName ++ "\" is already bound.")

checkIsHypothesisM :: String -> M ()
checkIsHypothesisM hypName = do
  b <- nameIsHypothesisM hypName
  if b
   then return ()
   else failM ("\"" ++ hypName ++ "\" should be the name of a hypothesis.")

checkIsNatVariableM :: String -> M ()
checkIsNatVariableM natVarName = do
  b <- nameIsNatVariableM natVarName
  if b
   then return ()
   else failM ("\"" ++ natVarName ++ "\" should be the name of a nat-variable.")

checkIsFormVariableM :: String -> M ()
checkIsFormVariableM formVarName = do
  b <- nameIsFormVariableM formVarName
  if b
   then return ()
   else failM ("\"" ++ formVarName ++ "\" should be the name of a prop-variable.")

----

bindHypothesesM :: [String] -> M ()
bindHypothesesM hypNames = mapM_ bindHypothesisM hypNames

bindHypothesisM :: String -> M ()
bindHypothesisM hypName =
  if hypName == "_"
   then return ()
   else do
     checkIsUnboundM hypName
     state <- getFS
     putFS (state { sHypotheses = S.insert hypName (sHypotheses state) })

unbindHypothesesM :: [String] -> M ()
unbindHypothesesM hypNames = mapM_ unbindHypothesisM hypNames

unbindHypothesisM :: String -> M ()
unbindHypothesisM hypName = do
  if hypName == "_"
   then return ()
   else do
     checkIsHypothesisM hypName
     state <- getFS
     putFS (state { sHypotheses = S.delete hypName (sHypotheses state) })

----

bindNatVariablesM :: [(NatVarKind, NatVarId)] -> M ()
bindNatVariablesM params =
  mapM_ (\ (k, n) -> bindNatVariableM (natVarIdName n) k) params

bindNatVariableM :: String -> NatVarKind -> M ()
bindNatVariableM natVarName kind = do
  checkIsUnboundM natVarName
  state <- getFS
  putFS (state { sNatVariables = M.insert natVarName kind (sNatVariables state) })

unbindNatVariablesM :: [(NatVarKind, NatVarId)] -> M ()
unbindNatVariablesM params =
  mapM_ (\ (_, n) -> unbindNatVariableM (natVarIdName n)) params

unbindNatVariableM :: String -> M ()
unbindNatVariableM natVarName = do
  checkIsNatVariableM natVarName
  state <- getFS
  putFS (state { sNatVariables = M.delete natVarName (sNatVariables state) })

----

bindFormVariableM :: String -> M ()
bindFormVariableM formVarName = do
  checkIsUnboundM formVarName
  state <- getFS
  putFS (state { sFormVariables = S.insert formVarName (sFormVariables state) })

unbindFormVariableM :: String -> M ()
unbindFormVariableM formVarName = do
  checkIsFormVariableM formVarName
  state <- getFS
  putFS (state { sFormVariables = S.delete formVarName (sFormVariables state) })

----

bindPropParamsM :: [PropParam] -> M ()
bindPropParamsM params = mapM_ bindPropParamM params

unbindPropParamsM :: [PropParam] -> M ()
unbindPropParamsM params = mapM_ unbindPropParamM params

bindPropParamM :: PropParam -> M ()
bindPropParamM (PropParamNat natVarId)   = bindNatVariableM (natVarIdName natVarId) NatVarSym
bindPropParamM (PropParamForm formVarId) = bindFormVariableM (formVarIdName formVarId)

unbindPropParamM :: PropParam -> M ()
unbindPropParamM (PropParamNat natVarId)   = unbindNatVariableM (natVarIdName natVarId)
unbindPropParamM (PropParamForm formVarId) = unbindFormVariableM (formVarIdName formVarId)

----

bindFunctionSymbolM :: String -> Integer -> M ()
bindFunctionSymbolM funIdName arity = do
  checkIsUnboundM funIdName
  state <- getFS
  putFS (state { sFunctionSymbols = M.insert funIdName arity (sFunctionSymbols state) })

bindPropSymbolM :: String -> [PropParam] -> M ()
bindPropSymbolM propIdName params = do
  checkIsUnboundM propIdName
  state <- getFS
  putFS (state { sPropSymbols = M.insert propIdName params (sPropSymbols state) })

getPropSymbolParamsM :: String -> M [PropParam]
getPropSymbolParamsM name = do
  state <- getFS
  return (M.findWithDefault (error "impossible") name (sPropSymbols state))

----

tokenStartsProofOuter :: Token -> Bool
tokenStartsProofOuter TSuppose  = True
tokenStartsProofOuter TAssume   = True
tokenStartsProofOuter TLet      = True
tokenStartsProofOuter TShow     = True
tokenStartsProofOuter TClaim    = True
tokenStartsProofOuter TThen     = True
tokenStartsProofOuter THave     = True
tokenStartsProofOuter TTake     = True
tokenStartsProofOuter TConsider = True
tokenStartsProofOuter _         = False

tokenStartsProofInner :: Token -> Bool
tokenStartsProofInner tok
  | tokenStartsProofOuter tok        = True
tokenStartsProofInner TLParen        = True
tokenStartsProofInner TIndeed        = True
tokenStartsProofInner TInduction     = True
tokenStartsProofInner TContradiction = True
tokenStartsProofInner (TId id)       = True
tokenStartsProofInner (THole id)     = True
tokenStartsProofInner _              = False

----

parse :: [Token] -> Either String Program
parse tokens =
    case evalFS parseProgramM initialState of
      Left msg      -> Left msg
      Right program -> Right program
  where 
    initialState = ParserState {
        sTokens = tokens
      , sHypotheses = S.empty
      , sNatVariables = M.empty
      , sFormVariables = S.empty
      , sFunctionSymbols = M.empty
      , sPropSymbols = M.fromList [
                         ("true", [])
                       , ("false", [])
                       , ("not", [PropParamForm (FormVarId "X")])
                       ]
    }

peekToken :: M Token
peekToken = do
  state <- getFS
  case sTokens state of
    [] -> return TEof
    (tok : _) -> return tok

peek2Tokens :: M [Token]
peek2Tokens = do
  state <- getFS
  case sTokens state of
    []                -> return [TEof]
    [tok]             -> return [tok, TEof]
    (tok1 : tok2 : _) -> return [tok1, tok2]

nextToken :: M ()
nextToken = do
  state <- getFS
  case sTokens state of
    [] -> return ()
    (_ : toks) ->
      putFS (state { sTokens = toks })

match :: Token -> M ()
match expectedToken = do
  tok <- peekToken
  if tok == expectedToken
   then nextToken
   else failM ("Expected: " ++ show expectedToken ++ " but got: " ++ show tok)

parseProgramM :: M Program
parseProgramM = Program <$> parseDeclarations

parseDeclarations :: M [Declaration]
parseDeclarations = do
  tok <- peekToken
  case tok of
    TEof -> return []
    _ -> do
      decl <- parseDeclaration
      decls <- parseDeclarations
      return (decl : decls)

parseDeclaration :: M Declaration
parseDeclaration = do
  tok <- peekToken
  case tok of
    TTheorem -> do
      decl <- parseDeclTheorem
      let DeclTheorem name _ _ = decl
      bindHypothesisM (hypIdName name)
      return decl
    TFun -> do
      decl <- parseDeclFun
      let DeclFun name params _ = decl
      bindFunctionSymbolM (funIdName name) (length' params)
      return decl
    TProp -> do
      decl <- parseDeclProp
      let DeclProp name params _ = decl
      bindPropSymbolM (propIdName name) params
      return decl
    TEval -> parseDeclEval
    tok -> failM ("Expected a declaration. Got: " ++ show tok)

parseId :: String -> (String -> a) -> M a
parseId description constructor = do
  tok <- peekToken
  case tok of
    TId id -> do
      nextToken
      return (constructor id)
    _ -> failM ("Expected a " ++ description ++ ". Got: " ++ show tok)

parseNatConst :: M Integer
parseNatConst = do
  tok <- peekToken
  case tok of
    TNatConst n -> do
      nextToken
      return n
    _ -> failM ("Expected a natural number constant. Got: " ++ show tok)

parseBareId :: M String
parseBareId = parseId "bare identifier" id

parseHypId :: M HypId
parseHypId = parseId "hypothesis identifier" HypId

parseNatVarId :: M NatVarId
parseNatVarId = parseId "natural number identifier" NatVarId

parseFunId :: M FunId
parseFunId = parseId "function identifier" FunId

parsePropId :: M PropId
parsePropId = parseId "proposition identifier" PropId

parseFormVarId :: M FormVarId
parseFormVarId = parseId "propositional variable" FormVarId

parseHashNatVarId :: M NatVarId
parseHashNatVarId = do
  match THash
  parseNatVarId

parseHoleId :: M HoleId
parseHoleId = do
  tok <- peekToken
  case tok of
    THole holeName -> do
      nextToken
      return (HoleId holeName)
    _ -> failM ("Expected a hole. Got: " ++ show tok)

parseDeclTheorem :: M Declaration
parseDeclTheorem = do
  match TTheorem
  theoremName <- parseHypId
  checkIsUnboundM (hypIdName theoremName)
  match TColon
  form <- parseForm
  match TProof
  proof <- parseProof
  match TEnd
  return $ DeclTheorem theoremName form proof

parseDeclFun :: M Declaration
parseDeclFun = do
  match TFun
  funcName <- parseFunId
  checkIsUnboundM (funIdName funcName)
  params <- parseParenList parseNatVarId
  let params' = zip (repeat NatVarSym) params
  match TDefEq
  bindNatVariablesM params'
  nat <- parseNat
  unbindNatVariablesM params'
  return $ DeclFun funcName params nat

parseDeclProp :: M Declaration
parseDeclProp = do
  match TProp
  propName <- parsePropId
  checkIsUnboundM (propIdName propName)
  params <- parseParenList parsePropParam
  match TDefEq
  bindPropParamsM params
  form <- parseForm
  unbindPropParamsM params
  return $ DeclProp propName params form

parseDeclEval :: M Declaration
parseDeclEval = do
  match TEval
  expr <- parsePropArg
  case expr of
    PropArgForm form -> return (DeclEvalForm form)
    PropArgNat nat -> return (DeclEvalNat nat)

parsePropParam :: M PropParam
parsePropParam = do
  tok <- peekToken
  case tok of
    TId name | isLowerName name -> PropParamNat <$> parseNatVarId
    TId name | isUpperName name -> PropParamForm <$> parseFormVarId
    _ -> failM ("Expected a parameter. Got: " ++ show tok)

parseParenList :: M a -> M [a]
parseParenList parseElem = do
    tok <- peekToken
    if tok /= TLParen
     then return []
     else do
       match TLParen
       tok' <- peekToken
       if tok' == TRParen
        then do match TRParen
                return []
        else do elems <- rec parseElem
                match TRParen
                return elems
  where
    rec :: M a -> M [a]
    rec parseElem = do
      x <- parseElem
      tok <- peekToken
      if tok == TComma
       then do match TComma
               xs <- rec parseElem
               return (x : xs)
       else return [x]

parsePropArg :: M PropArg
parsePropArg = parsePropArgInner formLevel

parseForm :: M Form
parseForm = do
  propArg <- parsePropArgInner formLevel
  checkPropArgIsForm propArg

parseNat :: M Nat
parseNat = do
  propArg <- parsePropArgInner natLevel
  checkPropArgIsNat propArg

checkPropArgIsForm :: PropArg -> M Form
checkPropArgIsForm (PropArgForm form) = return form
checkPropArgIsForm (PropArgNat nat) =
  failM ("Expected a formula. Got: " ++ pprintNat nat)

checkPropArgIsNat :: PropArg -> M Nat
checkPropArgIsNat (PropArgNat nat) = return nat
checkPropArgIsNat (PropArgForm form) =
  failM ("Expected a natural number. Got: " ++ pprintForm form)

formLevel :: Integer
formLevel = 0

natLevel :: Integer
natLevel = 10

parseFormBinaryOperator :: Token -> (Form -> Form -> Form) -> Integer -> M PropArg
parseFormBinaryOperator operatorToken formConstructor level = do
  pa <- parsePropArgInner (level + 1)
  tok <- peekToken
  if tok == operatorToken
   then do
      form1 <- checkPropArgIsForm pa
      match operatorToken
      form2 <- checkPropArgIsForm =<< parsePropArgInner level
      return (PropArgForm (formConstructor form1 form2))
   else return pa

parseNatBinaryOperator :: Token -> (Nat -> Nat -> Nat) -> Integer -> M PropArg
parseNatBinaryOperator operatorToken natConstructor level = do
  pa <- parsePropArgInner (level + 1)
  tok <- peekToken
  if tok == operatorToken
   then do
      form1 <- checkPropArgIsNat pa
      match operatorToken
      form2 <- checkPropArgIsNat =<< parsePropArgInner level
      return (PropArgNat (natConstructor form1 form2))
   else return pa

parsePropArgInner :: Integer -> M PropArg
-- Formulas --
parsePropArgInner level@0 = parseFormBinaryOperator TFormImp FormImp level
parsePropArgInner level@1 = parseFormBinaryOperator TFormOr FormOr level
parsePropArgInner level@2 = parseFormBinaryOperator TFormAnd FormAnd level
parsePropArgInner level@3 = do
  tok <- peekToken
  case tok of
    TForall -> do
      match TForall
      params <- parseNatVarParameterList
      bindNatVariablesM params
      match TComma
      body <- parseForm
      unbindNatVariablesM params
      return (PropArgForm (foldr (uncurry FormForall) body params))
    TExists -> do
      match TExists
      params <- parseNatVarParameterList
      bindNatVariablesM params
      match TComma
      body <- parseForm
      unbindNatVariablesM params
      return (PropArgForm (foldr (uncurry FormExists) body params))
    _ -> do
      pa <- parsePropArgInner (level + 1)
      tok <- peekToken
      case tok of
        TEq -> do
          nat1 <- checkPropArgIsNat pa
          match TEq
          nat2 <- checkPropArgIsNat =<< parsePropArgInner (level + 1)
          return (PropArgForm (FormEq nat1 nat2))
        _ -> return pa
parsePropArgInner level@4 = parsePropArgInner natLevel
-- Nats --
parsePropArgInner level@10 = parseNatBinaryOperator TAdd NatAdd level
parsePropArgInner level@11 = parseNatBinaryOperator TMul NatMul level
parsePropArgInner level@12 = parsePropArgAtom
parsePropArgInner _ = error "Invalid precedence level."

parseNatVarParameter :: M (NatVarKind, NatVarId)
parseNatVarParameter = do
  tok <- peekToken
  case tok of
    THash -> do
      id <- parseHashNatVarId
      return (NatVarFin, id)
    TId name | isLowerName name -> do
      id <- parseNatVarId
      return (NatVarSym, id)
    TId name -> do
      failM ("Natural number identifier must start with lowercase. Got: " ++ name)
    _ -> failM ("Expected a natural number parameter. Got: " ++ show tok)

parseNatVarParameterList :: M [(NatVarKind, NatVarId)]
parseNatVarParameterList = do
  tok <- peekToken
  if tok == THash || tokenIsId tok
   then do
     param  <- parseNatVarParameter
     params <- parseNatVarParameterList
     return (param : params)
   else
     return []

parseMaybeForm :: M (Maybe Form)
parseMaybeForm = do
  tok <- peekToken
  if tok == TColon
   then do match TColon
           form <- parseForm
           return (Just form)
   else return Nothing

parseHypothesis :: M (HypId, Maybe Form)
parseHypothesis = do
  tok <- peekToken
  case tok of
    TId _ -> do
      id <- parseHypId
      mForm <- parseMaybeForm
      return (id, mForm)
    TLParen -> do
      match TLParen
      id <- parseHypId
      match TColon
      form <- parseForm
      match TRParen
      return (id, Just form)
    _ -> failM ("Expected a hypothesis. Got: " ++ show tok)

parseHypothesisList :: M [(HypId, Maybe Form)]
parseHypothesisList = do
  tok <- peekToken
  if tok == TLParen || tokenIsId tok
   then do
     hyp <- parseHypothesis
     hyps <- parseHypothesisList
     return (hyp : hyps)
   else
     return []

parsePropArgAtom :: M PropArg
parsePropArgAtom = do
  tok <- peekToken
  case tok of
    TId _ -> do
      id <- parseBareId
      isFun <- nameIsFunctionSymbolM id
      isProp <- nameIsPropSymbolM id
      isFormVar <- nameIsFormVariableM id
      isNatVar  <- nameIsNatVariableM id
      case () of
        () | isFormVar -> do
           return (PropArgForm (FormVar (FormVarId id)))
        () | isNatVar -> do
           return (PropArgNat (NatVar (NatVarId id)))
        () | isProp -> do
           let propId = PropId id
           args <- parseParenList parsePropArg
           params <- getPropSymbolParamsM id
           if paramsMatchArgs params args
            then return (PropArgForm (FormProp propId args))
            else failM ("Parameter mismatch: " ++ id
                       ++ " takes parameters: " ++ pprintPropParams params ++ ". "
                       ++ "Got: " ++ pprintPropArgs args)
        () | isFun -> do
           let funId = FunId id
           args <- parseParenList parseNat
           arity <- getFunctionSymbolArityM id
           if length' args /= arity
            then failM ("Arity mismatch: " ++ id ++ " takes " ++ show arity ++ " parameters.")
            else return (PropArgNat (NatFun funId args))
        _ -> failM ("Identifier: '" ++ id ++ "' does not represent a formula or a natural number.")
    TNatConst _ -> do
      n <- parseNatConst
      return (PropArgNat (foldr (const NatSucc) NatZero [0..(n - 1)]))
    TSucc -> do
      match TSucc
      match TLParen
      nat <- parseNat
      match TRParen
      return (PropArgNat (NatSucc nat))
    TLParen -> do
      match TLParen
      propArg <- parsePropArg
      match TRParen
      return propArg
    THole _ -> do
      holeId <- parseHoleId
      return (PropArgNat (NatHole holeId))
    _ -> failFS "Invalid atomic formula"
  where
    paramsMatchArgs :: [PropParam] -> [PropArg] -> Bool
    paramsMatchArgs params args =
         length params == length args
      && and (zipWith paramMatchArg params args)
    paramMatchArg :: PropParam -> PropArg -> Bool
    paramMatchArg (PropParamNat _)  (PropArgNat _) = True
    paramMatchArg (PropParamForm _) (PropArgForm _) = True
    paramMatchArg _ _ = False

parseProof :: M Proof
parseProof = parseProofOuter

parseProofOuter :: M Proof
parseProofOuter = do
    tok <- peekToken
    case tok of
      TLet      -> parseLet
      TSuppose  -> parseSuppose
      TAssume   -> parseAssume
      TShow     -> parseShow
      TClaim    -> parseClaim
      TThen     -> parseThen
      THave     -> parseHave
      TCases    -> parseCases
      TTake     -> parseTake
      TConsider -> parseConsider
      _ -> parseProofApply
  where
    parseLet :: M Proof
    parseLet = do
      match TLet
      params <- parseNatVarParameterList
      match TComma
      bindNatVariablesM params
      body <- parseProofOuter
      unbindNatVariablesM params
      return (foldr (uncurry proofLet) body params)
    parseSuppose :: M Proof
    parseSuppose = do
      match TSuppose
      hyps <- parseHypothesisList
      let hypNames = map (hypIdName . fst) hyps
      match TComma
      bindHypothesesM hypNames
      body <- parseProofOuter
      unbindHypothesesM hypNames
      return (foldr (uncurry ProofSuppose) body hyps)
    parseAssume :: M Proof
    parseAssume = do
      match TAssume
      hyps <- parseHypothesisList
      let hypNames = map (hypIdName . fst) hyps
      match TComma
      bindHypothesesM hypNames
      body <- parseProofOuter
      unbindHypothesesM hypNames
      return (ProofAssume hyps body)
    parseShow :: M Proof
    parseShow = do
      match TShow
      form <- parseForm
      match TComma
      body <- parseProofOuter
      return (ProofShow form body)
    parseClaim :: M Proof
    parseClaim = do
      match TClaim
      hypId <- parseHypId
      let hypName = hypIdName hypId
      match TColon
      cutForm <- parseForm
      match TProof
      cutProof <- parseProof
      match TEnd
      bindHypothesisM hypName
      mainProof <- parseProof
      unbindHypothesisM hypName
      return (ProofClaim hypId cutForm cutProof mainProof)
    parseThen :: M Proof
    parseThen = do
        match TThen
        proof <- parseProof
        proofs <- recAlso
        return (ProofThen (proof : proofs))
      where
        recAlso = do
          tok <- peekToken
          case tok of
            TAlso -> do
              match TAlso
              proof <- parseProof
              proofs <- recAlso
              return (proof : proofs)
            _ -> return []
    parseHave :: M Proof
    parseHave = do
      match THave
      hyps <- parseHypothesisList
      let hypNames = map (hypIdName . fst) hyps
      match TBy
      proofConj <- parseProof
      match TComma
      bindHypothesesM hypNames
      proofBody <- parseProof
      unbindHypothesesM hypNames
      return (ProofHave hyps proofConj proofBody)
    parseCases :: M Proof
    parseCases = do
      match TCases
      proof <- parseProof
      branches <- parseCaseBranches
      return (ProofCases proof branches)
    parseCaseBranches :: M [CaseBranch]
    parseCaseBranches = do
      tok <- peekToken
      case tok of
        TCase -> do
          match TCase
          hypId <- parseHypId
          let hypName = hypIdName hypId
          mForm <- parseMaybeForm      
          match TComma
          bindHypothesisM hypName
          proof <- parseProof
          unbindHypothesisM hypName
          let branch = CaseBranch hypId mForm proof
          branches <- parseCaseBranches
          return (branch : branches)
        _ -> return []

    parseTake :: M Proof
    parseTake = do
        match TTake
        nats <- parseNatList
        match TComma
        proof <- parseProof
        return (foldr ProofTake proof nats)
      where
        parseNatList :: M [Nat]
        parseNatList = do
          tok <- peekToken
          if tok == TComma
           then return []
           else (:) <$> parseNat <*> parseNatList

    parseConsider :: M Proof
    parseConsider = do
      match TConsider
      params <- parseNatVarParameterList
      match TSt
      bindNatVariablesM params
      hypId <- parseHypId
      let hypName = hypIdName hypId
      mForm <- parseMaybeForm
      match TBy
      proof1 <- parseProof
      match TComma
      bindHypothesisM hypName
      proof2 <- parseProof
      unbindHypothesisM hypName
      unbindNatVariablesM params
      return (ProofConsider params hypId mForm proof1 proof2)
    parseProofApply :: M Proof
    parseProofApply = do
      app <- parseProofApplicationList
      case app of
        [] -> failM "Premature end of proof."
        (Right _ : _) -> failM "A natural number cannot be applied."
        (Left proof : args) -> return (foldl proofApply proof args)

parseProofApplicationList :: M [Either Proof Nat]
parseProofApplicationList = do
  tok <- peekToken
  case tok of
    _ | tokenStartsProofOuter tok -> do
      proof <- parseProofOuter
      return [Left proof]
    _ | tokenStartsProofInner tok -> do
      proof <- parseProofAtom
      app <- parseProofApplicationList
      return (Left proof : app)
    _ | tok == TLBrack -> do
      match TLBrack
      nat <- parseNat
      match TRBrack
      app <- parseProofApplicationList
      return (Right nat : app)
    _ -> return []

parseProofAtom :: M Proof
parseProofAtom = do
  tok <- peekToken
  case tok of
    THole _ -> do
      holeId <- parseHoleId
      return (ProofHole holeId)
    TId _ -> do
      id <- parseHypId
      checkIsHypothesisM (hypIdName id)
      return (ProofAx id)
    TIndeed -> parseProofIndeed
    TInduction -> parseProofInduction
    TContradiction -> parseProofContradiction
    TLParen -> do
      match TLParen
      proof <- parseProof
      match TRParen
      return proof
    _ -> failM ("Expected a proof. Got: " ++ show tok)
 
parseProofIndeed :: M Proof
parseProofIndeed = do
  match TIndeed
  eqChain <- parseEqChain
  case eqChain of 
    -- Special case: "indeed ?" produces just a hole "?".
    -- This is just to improve UX.
    EqRefl (NatHole holeName) -> return (ProofHole holeName)
    _ -> return (ProofIndeed eqChain)

parseProofInduction :: M Proof
parseProofInduction = do
  match TInduction
  base <- parseProofAtom
  step <- parseProofOuter
  return (ProofInduction base step)

parseProofContradiction :: M Proof
parseProofContradiction = do
  match TContradiction
  proof <- parseProofOuter
  return (ProofContradiction proof)

parseEqChain :: M EqChain
parseEqChain = do
  nat <- parseNat
  parseEqChain1 nat

parseEqChain1 :: Nat -> M EqChain
parseEqChain1 nat1 = do
  tok <- peekToken
  if tok == TEq
   then do
     match TEq
     next2Toks <- peek2Tokens
     (nat2, justification) <-
         case next2Toks of
           [TLParen, TBy] -> do
             --   nat1 =(by justif) nat2
             match TLParen
             match TBy
             justification <- Just <$> parseProof
             match TRParen
             nat2 <- parseNat
             return (nat2, justification)
           _ -> do
             --   nat1 = nat2
             --   nat1 = nat2 by justif
             --   nat1 = nat2 (by justif)
             nat2 <- parseNat
             justification <- parseEqJustification
             return (nat2, justification)
     eqChain <- parseEqChain1 nat2
     return (EqTrans nat1 justification eqChain)
   else return (EqRefl nat1)

parseEqJustification :: M (Maybe Proof)
parseEqJustification = do
  next2Toks <- peek2Tokens
  case next2Toks of
    [TLParen, TBy] -> do
      match TLParen
      match TBy
      proof <- parseProof
      match TRParen
      return (Just proof)
    [TBy, _] -> do
      match TBy
      proof <- parseProof
      return (Just proof)
    _ -> return Nothing

