module Compiler where

import Data.List(union, (\\), nub, elemIndex)
import Control.Monad.State(State, evalState, get, put)

import AST(Id(..), Program(..), Definition(..), Expr(..), CaseBranch(..))

data CompilerState = CompilerState {
                       csNextFreshId      :: Int
                     , csNextFreshRoutine :: Int
                     , csNextFreshLabel   :: Int
                     }

type M a = State CompilerState a

type Code = [String]

compileProgram :: Program -> String
compileProgram program =
    unlines $ evalState (compileProgramM program) initialState
  where initialState = CompilerState {
                         csNextFreshId      = 0
                       , csNextFreshRoutine = 0
                       , csNextFreshLabel   = 0
                       }

tagStructure :: Int
tagStructure = undefined
tagInt       = 1
tagChar      = 2
tagClosure   = 3
basicConstructors = [
   "<NONE>", "Int", "Char", "Closure", "True", "False"
 ]

type Regname = String
type Label = String
data Register = RLocal Regname

data Environment = Env {
                     envBindings :: [(Id, Binding)],
                     envConstructors :: [Id]
                   }

extendEnv :: [(Id, Binding)] -> Environment -> Environment
extendEnv bs env = env { envBindings = bs ++ envBindings env }

lookupEnv :: Id -> Environment -> Maybe Binding
lookupEnv x env = lookup x (envBindings env)

tagForConstructor :: Id -> Environment -> Int
tagForConstructor c env =
  case elemIndex c (envConstructors env) of
    Nothing -> error ("Constructor desconocido: " ++ c)
    Just i  -> i

data Binding = BGlobal Regname
             | BLocal Regname
             | BEnclosed Int
             | BPrimitive

globalName :: Id -> Regname
globalName x = "@G_" ++ x

freshVariable :: M Regname
freshVariable = do
  state <- get
  put (state { csNextFreshId = csNextFreshId state + 1})
  return ("$r" ++ show (csNextFreshId state))

freshRoutine :: M Regname
freshRoutine = do
  state <- get
  put (state { csNextFreshRoutine = csNextFreshRoutine state + 1})
  return ("rtn" ++ show (csNextFreshRoutine state))

freshLabel :: M Label
freshLabel = do
  state <- get
  put (state { csNextFreshLabel = csNextFreshLabel state + 1})
  return ("L" ++ show (csNextFreshLabel state))

----

freeVariables :: Environment -> Expr -> [Id]
freeVariables env (ExprVar x) =
  case lookupEnv x env of
    Nothing            -> [x]
    Just (BGlobal   _) -> []
    Just (BLocal    _) -> [x]
    Just (BEnclosed _) -> [x]
    Just BPrimitive    -> []
freeVariables env (ExprConstructor _)       = []
freeVariables env (ExprNumber _)            = []
freeVariables env (ExprChar _)              = []
freeVariables env (ExprCase e branches)     = freeVariables env e `union`
                                               nub (concatMap fvBranch branches)
  where fvBranch (CaseBranch _ xs e) = freeVariables env e \\ xs
freeVariables env (ExprLet x e1 e2)         = freeVariables env e1 `union`
                                              (freeVariables env e2 \\ [x])
freeVariables env (ExprLambda x e)          = freeVariables env e \\ [x]
freeVariables env (ExprApply e1 e2)         = freeVariables env e1 `union`
                                              freeVariables env e2

----

collectDefConstructors :: Definition -> [Id]
collectDefConstructors (Def x expr) = collectExprConstructors expr

collectExprConstructors :: Expr -> [Id]
collectExprConstructors (ExprVar _)               = []
collectExprConstructors (ExprConstructor x)       = [x]
collectExprConstructors (ExprNumber _)            = []
collectExprConstructors (ExprChar _)              = []
collectExprConstructors (ExprCase e branches)     =
    collectExprConstructors e `union`
    nub (concatMap collectBranchConstructors branches)
  where collectBranchConstructors (CaseBranch c _ e) =
          [c] `union` collectExprConstructors e
collectExprConstructors (ExprLet x e1 e2)         =
  collectExprConstructors e1 `union`
  collectExprConstructors e2
collectExprConstructors (ExprLambda x e)          =
  collectExprConstructors e
collectExprConstructors (ExprApply e1 e2)         =
  collectExprConstructors e1 `union`
  collectExprConstructors e2

----

compileProgramM :: Program -> M Code
compileProgramM (Program definitions) = do
  (globalRoutines, globalCode) <- initializeGlobalsM definitions
  return (
   ["  jump start"] ++
   globalRoutines ++
   ["start:"] ++
   globalCode
   )
  where
    globalEnv = Env {
                  envBindings = globalBindings,
                  envConstructors = globalConstructors
                }
    globalBindings =
      [
        ("unsafePrintInt", BPrimitive),
        ("unsafePrintChar", BPrimitive),
        ("AND", BPrimitive),
        ("OR", BPrimitive),
        ("NOT", BPrimitive),
        ("EQ", BPrimitive),
        ("NE", BPrimitive),
        ("LE", BPrimitive),
        ("GE", BPrimitive),
        ("LT", BPrimitive),
        ("GT", BPrimitive),
        ("ADD", BPrimitive),
        ("SUB", BPrimitive),
        ("MUL", BPrimitive),
        ("DIV", BPrimitive),
        ("MOD", BPrimitive),
        ("UMINUS", BPrimitive)
      ]
      ++
      [
        (x, BGlobal (globalName x))
      |
        Def x _ <- definitions
      ]
    globalConstructors =
      nub (basicConstructors ++ concatMap collectDefConstructors definitions)

    initializeGlobalsM :: [Definition] -> M (Code, Code)
    initializeGlobalsM []               = return ([], [])
    initializeGlobalsM (Def x e : defs) = do
      (routines1, code1) <- compileExprM globalEnv (RLocal (globalName x)) e
      (routines2, code2) <- initializeGlobalsM defs
      return (routines1 ++ routines2, code1 ++ code2)

compileExprM :: Environment -> Register -> Expr -> M (Code, Code)

compileExprM env (RLocal reg) (ExprApply (ExprVar "unsafePrintChar") e) = do
  tmpChr           <- freshVariable
  (routines, code) <- compileExprM env (RLocal reg) e
  return (routines, code ++ [
     "  load(" ++ tmpChr ++ ", " ++ reg ++ ", 1)",
     "  print_char(" ++ tmpChr ++ ")"
    ])

compileExprM env (RLocal reg) (ExprApply (ExprVar "unsafePrintInt") e) = do
  tmpInt           <- freshVariable
  (routines, code) <- compileExprM env (RLocal reg) e
  return (routines, code ++ [
     "  load(" ++ tmpInt ++ ", " ++ reg ++ ", 1)",
     "  print(" ++ tmpInt ++ ")"
    ])

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "OR") e1) e2) =
  let tFalse = tagForConstructor "False" env in do
     tmp <- freshVariable
     (routines1, code1) <- compileExprM env (RLocal reg) e1
     (routines2, code2) <- compileExprM env (RLocal reg) e2
     lend <- freshLabel
     return (
       routines1 ++ routines2,
       code1 ++
       [
         "  load(" ++ tmp ++ ", " ++ reg ++ ", 0)",
         "  mov_int($t, " ++ show tFalse ++ ")",
         "  jump_eq(" ++ tmp ++ ", $t, " ++ lend ++ ")"
       ] ++
       code2 ++
       [
         lend ++ ":"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "AND") e1) e2) =
  let tTrue = tagForConstructor "True" env in do
     tmp <- freshVariable
     (routines1, code1) <- compileExprM env (RLocal reg) e1
     (routines2, code2) <- compileExprM env (RLocal reg) e2
     lend <- freshLabel
     return (
       routines1 ++ routines2,
       code1 ++
       [
         "  load(" ++ tmp ++ ", " ++ reg ++ ", 0)",
         "  mov_int($t, " ++ show tTrue ++ ")",
         "  jump_eq(" ++ tmp ++ ", $t, " ++ lend ++ ")"
       ] ++
       code2 ++
       [
         lend ++ ":"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprVar "NOT") e) =
  let tTrue  = tagForConstructor "True" env
      tFalse = tagForConstructor "False" env
   in do
     (routines1, code1) <- compileExprM env (RLocal reg) e
     lTrue <- freshLabel
     lEnd <- freshLabel
     return (
       routines1,
       code1 ++
       [
         "  load(" ++ reg ++ ", " ++ reg ++ ", 0)",
         "  mov_int($t, " ++ show tTrue ++ ")",
         "  jump_eq(" ++ reg ++ ", $t, " ++ lTrue ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  jump(" ++ lEnd ++ ")",
         lTrue ++ ":",
         "  mov_int($t, " ++ show tFalse ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         lEnd ++ ":"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "EQ") e1) e2) =
  let tTrue  = tagForConstructor "True" env
      tFalse = tagForConstructor "False" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     lTrue <- freshLabel
     lEnd <- freshLabel
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  jump_eq(" ++ tmp1 ++ ", " ++ tmp2 ++ ", " ++ lTrue ++ ")",
         "  mov_int($t, " ++ show tFalse ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  jump(" ++ lEnd ++ ")",
         lTrue ++ ":",
         "  mov_int($t, " ++ show tTrue ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         lEnd ++ ":"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "NE") e1) e2) =
  compileExprM env (RLocal reg)
               (ExprApply (ExprVar "NOT")
                          (ExprApply (ExprApply (ExprVar "EQ") e1) e2))

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "GE") e1) e2) =
  compileExprM env (RLocal reg)
               (ExprApply (ExprVar "NOT")
                          (ExprApply (ExprApply (ExprVar "LT") e1) e2))

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "LE") e1) e2) =
  let tTrue  = tagForConstructor "True" env
      tFalse = tagForConstructor "False" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     lTrue <- freshLabel
     lEnd <- freshLabel
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  jump_lt(" ++ tmp1 ++ ", " ++ tmp2 ++ ", " ++ lTrue ++ ")",
         "  jump_eq(" ++ tmp1 ++ ", " ++ tmp2 ++ ", " ++ lTrue ++ ")",
         "  mov_int($t, " ++ show tFalse ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  jump(" ++ lEnd ++ ")",
         lTrue ++ ":",
         "  mov_int($t, " ++ show tTrue ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         lEnd ++ ":"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "GT") e1) e2) =
  compileExprM env (RLocal reg)
               (ExprApply (ExprVar "NOT")
                          (ExprApply (ExprApply (ExprVar "LE") e1) e2))

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "LT") e1) e2) =
  let tTrue  = tagForConstructor "True" env
      tFalse = tagForConstructor "False" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     lTrue <- freshLabel
     lEnd <- freshLabel
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  jump_lt(" ++ tmp1 ++ ", " ++ tmp2 ++ ", " ++ lTrue ++ ")",
         "  mov_int($t, " ++ show tFalse ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  jump(" ++ lEnd ++ ")",
         lTrue ++ ":",
         "  mov_int($t, " ++ show tTrue ++ ")",
         "  alloc(" ++ reg ++ ", 1)",
         "  store(" ++ reg ++ ", 0, $t)",
         lEnd ++ ":"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "ADD") e1) e2) =
  let tInt  = tagForConstructor "Int" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  add(" ++ tmp1 ++ ", " ++ tmp1 ++ ", " ++ tmp2 ++ ")",
         "  mov_int($t, " ++ show tInt ++ ")",
         "  alloc(" ++ reg ++ ", 2)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  store(" ++ reg ++ ", 1, " ++ tmp1 ++ ")"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "SUB") e1) e2) =
  let tInt  = tagForConstructor "Int" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  sub(" ++ tmp1 ++ ", " ++ tmp1 ++ ", " ++ tmp2 ++ ")",
         "  mov_int($t, " ++ show tInt ++ ")",
         "  alloc(" ++ reg ++ ", 2)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  store(" ++ reg ++ ", 1, " ++ tmp1 ++ ")"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "MUL") e1) e2) =
  let tInt  = tagForConstructor "Int" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  mul(" ++ tmp1 ++ ", " ++ tmp1 ++ ", " ++ tmp2 ++ ")",
         "  mov_int($t, " ++ show tInt ++ ")",
         "  alloc(" ++ reg ++ ", 2)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  store(" ++ reg ++ ", 1, " ++ tmp1 ++ ")"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "DIV") e1) e2) =
  let tInt  = tagForConstructor "Int" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  div(" ++ tmp1 ++ ", " ++ tmp1 ++ ", " ++ tmp2 ++ ")",
         "  mov_int($t, " ++ show tInt ++ ")",
         "  alloc(" ++ reg ++ ", 2)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  store(" ++ reg ++ ", 1, " ++ tmp1 ++ ")"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprApply (ExprVar "MOD") e1) e2) =
  let tInt  = tagForConstructor "Int" env
   in do
     tmp1 <- freshVariable
     tmp2 <- freshVariable
     (routines1, code1) <- compileExprM env (RLocal tmp1) e1
     (routines2, code2) <- compileExprM env (RLocal tmp2) e2
     return (
       routines1 ++ routines2,
       code1 ++ code2 ++
       [
         "  load(" ++ tmp1 ++ ", " ++ tmp1 ++ ", 1)",
         "  load(" ++ tmp2 ++ ", " ++ tmp2 ++ ", 1)",
         "  mod(" ++ tmp1 ++ ", " ++ tmp1 ++ ", " ++ tmp2 ++ ")",
         "  mov_int($t, " ++ show tInt ++ ")",
         "  alloc(" ++ reg ++ ", 2)",
         "  store(" ++ reg ++ ", 0, $t)",
         "  store(" ++ reg ++ ", 1, " ++ tmp1 ++ ")"
       ]
      )

compileExprM env (RLocal reg)
                 (ExprApply (ExprVar "UMINUS") e) =
  compileExprM env (RLocal reg)
               (ExprApply (ExprApply (ExprVar "SUB") (ExprNumber 0)) e)

compileExprM env (RLocal reg) (ExprVar x) =
  case lookupEnv x env of
    Nothing             -> error ("Variable no ligada: " ++ x)
    Just (BGlobal greg) ->
      return ([], [
          "  mov_reg(" ++ reg ++ ", " ++ greg ++ ")"
        ])
    Just (BLocal lreg) ->
      return ([], [
          "  mov_reg(" ++ reg ++ ", " ++ lreg ++ ")"
        ])
    Just (BEnclosed i) -> do
      tmpOffset <- freshVariable
      tmpValue  <- freshVariable
      return ([], [
          "  load(" ++ reg ++ ", $fun, " ++ show (i + 2) ++ ")"
        ])
    Just BPrimitive ->
      error "Las primitivas no se pueden usar como variables."
    
compileExprM env (RLocal reg) (ExprNumber n) = do
  return ([],
    [ 
      "  alloc(" ++ reg ++ ", 2)",
      "  mov_int($t, " ++ show tagInt ++ ")",
      "  store(" ++ reg ++ ", 0, $t)",
      "  mov_int($t, " ++ show n ++ ")",
      "  store(" ++ reg ++ ", 1, $t)"
    ])
compileExprM env (RLocal reg) (ExprChar n) =
  return ([],
    [ 
      "  alloc(" ++ reg ++ ", 2)",
      "  mov_int($t, " ++ show tagChar ++ ")",
      "  store(" ++ reg ++ ", 0, $t)",
      "  mov_int($t, " ++ show n ++ ")",
      "  store(" ++ reg ++ ", 1, $t)"
    ])

compileExprM env (RLocal reg) (ExprConstructor cons) = do
  let tag = tagForConstructor cons env in
    return (
      [],
      [
        "alloc(" ++ reg ++ ", 1)",
        "mov_int($t, " ++ show tag ++ ")",
        "store(" ++ reg ++ ", 0, $t)"
      ])

compileExprM env (RLocal reg) (ExprLet x e1 e2) = do
  tmp <- freshVariable
  (routines1, code1) <- compileExprM env (RLocal tmp) e1
  (routines2, code2) <- compileExprM (extendEnv [(x, BLocal tmp)] env)
                                     (RLocal reg)
                                     e2
  return (routines1 ++ routines2, code1 ++ code2)
compileExprM env (RLocal reg) (ExprLambda x e) = do
  routine <- freshRoutine
  returnVal <- freshVariable
  let enclosedVars = freeVariables env e \\ [x]
      enclosedBindings = [(y, BEnclosed i) | (y, i) <- zip enclosedVars [0..]]
      extendedEnv = extendEnv ((x, BLocal "$arg") : enclosedBindings) env
   in do
    (routines1, code) <- compileExprM extendedEnv (RLocal returnVal) e
    (routines2, storeEnclosed) <-
      storeEnclosedVars reg env enclosedVars [0..]
    return (
      routines1 ++ routines2 ++ [
        routine ++ ":",
        "  mov_reg($fun, @fun)",
        "  mov_reg($arg, @arg)"
      ] ++ code ++ [
        "  mov_reg(@res, " ++ returnVal ++ ")",
        "  return()"
      ],
      [
        -- Reservar espacio para n + 2 celdas
        "  alloc(" ++ reg ++ ", " ++ show (length enclosedVars + 2) ++ ")",
        "  mov_int($t, " ++ show tagClosure ++ ")",
        "  store(" ++ reg ++ ", 0, $t)",        -- tag
        "  mov_label($t, " ++ routine ++ ")",
        "  store(" ++ reg ++ ", 1, $t)"         -- function pointer
      ] ++
      storeEnclosed)
  where
    storeEnclosedVars reg env []       _        = return ([], [])
    storeEnclosedVars reg env (v : vs) (i : is) = do
      tmpVarVal      <- freshVariable
      (routines1, code1) <- compileExprM env (RLocal tmpVarVal) (ExprVar v)
      (routines2, code3) <- storeEnclosedVars reg env vs is
      return (routines1 ++ routines2,
              code1 ++ [
                "  store(" ++ reg
                   ++ ", " ++ show (i + 2)
                   ++ ", " ++ tmpVarVal ++ ")"
              ] ++ code3)

compileExprM env (RLocal reg) (ExprApply fun arg)
  | isStructure fun =
    let cons = structureCons fun
        tag  = tagForConstructor cons env
        args = structureArgs (ExprApply fun arg)
        n    = length args
     in do
       tmpArgs <- mapM (\_ -> freshVariable) args
       (routines, code) <- compileExprsM env (map RLocal tmpArgs) args
       return (
         routines,
         code ++ [
           "  alloc(" ++ reg ++ ", " ++ show (1 + n) ++ ")",
           "  mov_int($t, " ++ show tag ++ ")",
           "  store(" ++ reg ++ ", 0, $t)"
         ] ++ [
           "  store(" ++ reg ++ ", " ++ show i ++ ", " ++ ra ++ ")"
         |
           (i, ra) <- zip [1..] tmpArgs
         ])
  | otherwise       = do
    tmpFun    <- freshVariable
    tmpArg    <- freshVariable
    tmpFunPtr <- freshVariable
    (routines1, code1)  <- compileExprM env (RLocal tmpFun) fun
    (routines2, code2)  <- compileExprM env (RLocal tmpArg) arg
    return (
      routines1 ++ routines2,
      code1 ++
      code2 ++ [
        "  mov_reg(@fun, " ++ tmpFun ++ ")",
        "  mov_reg(@arg, " ++ tmpArg ++ ")",
        "  load(" ++ tmpFunPtr ++ ", @fun, 1)",
        "  icall(" ++ tmpFunPtr ++ ")",
        "  mov_reg(" ++ reg ++ ", @res)"
      ])
  where
    isStructure (ExprApply f _)     = isStructure f
    isStructure (ExprConstructor _) = True
    isStructure _                   = False
    structureCons (ExprApply f _)     = structureCons f
    structureCons (ExprConstructor c) = c
    structureArgs (ExprApply f x)     = structureArgs f ++ [x]
    structureArgs (ExprConstructor _) = []

compileExprM env (RLocal reg) (ExprCase e branches) = do
  let branchTags = map branchTag branches in do
    branchLabels  <- mapM (\ _ -> freshLabel) branches
    endLabel      <- freshLabel
    tmpSubject    <- freshVariable
    tmpSubjectTag <- freshVariable
    (routines1, code1)       <- compileExprM env (RLocal tmpSubject) e
    (routines2, branchCodes) <- compileBranches env tmpSubject
                                                    (RLocal reg) branches
    return (routines1 ++ routines2,
      code1 ++
      [
        "  load(" ++ tmpSubjectTag ++ ", " ++ tmpSubject ++ ", 0)"
      ] ++
      concat [
        [
          "  mov_int($t, " ++ show tag ++ ")",
          "  jump_eq(" ++ tmpSubjectTag ++ ", $t, " ++ label ++ ")"
        ]
      |
        (tag, label) <- zip branchTags branchLabels
      ] ++
      concat [
        [
          label ++ ":"
        ] ++ code ++ [
          "  jump " ++ endLabel
        ]
      |
        (label, code) <- zip branchLabels branchCodes
      ] ++
      [
        endLabel ++ ":"
      ])
  where
    branchTag (CaseBranch cons _ _) = tagForConstructor cons env 

compileBranches :: Environment -> Regname -> Register -> [CaseBranch] ->
                   M (Code, [Code])
compileBranches _   _    _ []                  = return ([], [])
compileBranches env subj r (branch : branches) = do
  (routines1, code)  <- compileBranch env subj r branch
  (routines2, codes) <- compileBranches env subj r branches
  return (routines1 ++ routines2, code : codes)

compileBranch :: Environment -> Regname -> Register -> CaseBranch ->
                   M (Code, Code)
compileBranch env subj r (CaseBranch _ args body) = do
  tmpArgs <- mapM (\ _ -> freshVariable) args
  let extendedEnv = extendEnv (zip args (map BLocal tmpArgs)) env
   in do
     (code1, routine1) <- compileExprM extendedEnv r body
     return (
       code1,
       [
         "  load(" ++ arg ++ ", " ++ subj ++ ", " ++ show i ++ ")"
       |
         (i, arg) <- zip [1..] tmpArgs
       ] ++
       routine1
       )

compileExprsM :: Environment -> [Register] -> [Expr] -> M (Code, Code)
compileExprsM _   []       [] = return ([], [])
compileExprsM env (r : rs) (e : es) = do
  (routines1, code1) <- compileExprM env r e
  (routines2, code2) <- compileExprsM env rs es
  return (routines1 ++ routines2, code1 ++ code2)
 
joinS :: [a] -> [[a]] -> [a]
joinS sep []       = []
joinS sep [x]      = x
joinS sep (x : xs) = x ++ sep ++ joinS sep xs

