module Compiler(compile) where

import Data.List(union, elemIndex, (\\))

import AST(
         Id, Type(..), ProgramT(..), FunctionT(..), ParameterT(..),
         BlockT(..), StmtT(..), ExprT(..)
       )
import FreeVars(freeVariables)

import Control.Monad.Trans.State(State, evalState, get, modify)

joinS :: String -> [String] -> String
joinS _ []   = ""
joinS sep xs = foldr1 (\ x r -> x ++ sep ++ r) xs

type Asm = String

type M = State S
data S = S {
           requiredPrimitiveList :: [Id],
           currentFunctionParameters :: [Id],
           currentFunctionLocals :: [Id],
           currentFunctionTemporaries :: Integer,
           nextLabelId :: Integer
         }

-- convention:
--  RDI, RSI, RDX, RCX, R8, R9
data Place =
    RDI
  | RSI
  | RAX
  | RBX
  | RCX
  | RDX
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | Temporary Integer
  deriving Eq

isRegister :: Place -> Bool
isRegister (Temporary _) = False
isRegister _             = True

instance Show Place where
  show RDI = "rdi"
  show RSI = "rsi"
  show RAX = "rax"
  show RBX = "rbx"
  show RCX = "rcx"
  show RDX = "rdx"
  show R8  = "r8"
  show R9  = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"
  show (Temporary n) = error "should not show"

putInFront :: Place -> [Place] -> [Place]
putInFront p ps = p : as ++ bs
  where (as, _ : bs) = span (/= p) ps

---- Helpers

nonempty :: [a] -> Bool
nonempty [] = False
nonempty _  = True

orEmpty :: Bool -> [a] -> [a]
orEmpty cond lst
  | cond      = lst
  | otherwise = []

---- Monad helpers

allAvailableRegisters :: [Place]
allAvailableRegisters =
    [RDI, RSI, RBX, RCX, R8, R9, R10, R11, R12, R13, R14, R15]
    -- RBP y RSP se reservan para base y stack pointer como de costumbre
    -- RAX y RDX se reservan para multiplicar
    -- RAX se usa para el return (?TODO)
    -- RDX se usa para pasar datos registro <-> memoria

allAvailablePlaces :: [Place]
allAvailablePlaces = allAvailableRegisters ++ map Temporary [1..]

currentlyUsedRegisters :: [Place] -> [Place]
currentlyUsedRegisters places =
  let (currentlyAvailableRegisters, _) = span isRegister places in
    allAvailableRegisters \\ currentlyAvailableRegisters

scratch :: Place
scratch = RDX

requirePrimitive :: Id -> M ()
requirePrimitive p =
  modify (\ s -> s {
                   requiredPrimitiveList =
                     [p] `union` requiredPrimitiveList s
                 })

getRequiredPrimitives :: M [Id]
getRequiredPrimitives = requiredPrimitiveList <$> get

enterFunction :: [Id] -> [Id] -> M ()
enterFunction currentFunctionParameters currentFunctionLocals =
  modify (\ s -> s {
                   currentFunctionParameters = currentFunctionParameters,
                   currentFunctionLocals = currentFunctionLocals,
                   currentFunctionTemporaries = 0
                 })

updateCurrentFunctionTemporaries :: Integer -> M ()
updateCurrentFunctionTemporaries k =
  modify (\ s -> s {
                   currentFunctionTemporaries =
                       max (currentFunctionTemporaries s) k
                 })

getCurrentFunctionParameters :: M [Id]
getCurrentFunctionParameters = currentFunctionParameters <$> get

getCurrentFunctionLocals :: M [Id]
getCurrentFunctionLocals = currentFunctionLocals <$> get

getCurrentFunctionTemporaries :: M Integer
getCurrentFunctionTemporaries = currentFunctionTemporaries <$> get

makeLabel :: M String
makeLabel = do
  modify (\ s -> s {
                   nextLabelId = nextLabelId s + 1
                 })
  id <- nextLabelId <$> get
  return $ ".label_" ++ show id

----

compile :: ProgramT -> Asm
compile program = joinS "\n" $ evalState (compileProgram program) init
  where init = S {
                 requiredPrimitiveList = [],
                 currentFunctionParameters = [],
                 currentFunctionLocals = [],
                 currentFunctionTemporaries = 0,
                 nextLabelId = 0
               }

compileProgram :: ProgramT -> M [Asm]
compileProgram (Program functions) = do
  cFunctions <- concat <$> mapM compileFunction functions
  requiredPrimitives <- getRequiredPrimitives
  let
   in
    return $
      dataSection requiredPrimitives ++
      [
      "section .text",
      "global main",
      "extern " ++ externDeclarations requiredPrimitives
      ] ++
      cFunctions ++ 
      [
      "main:",
      "    call cuca_main",
      "    mov " ++ show RDI ++ ", 0",
      "    call exit",
      ""
      ]
  where
    dataSection :: [Id] -> [Asm]
    dataSection requiredPrimitives
      | "putNum" `elem` requiredPrimitives =
         [
         "section .data",
         "lli_format_string db \"%lli\""
         ]
      | otherwise = []

    externDeclarations :: [Id] -> String
    externDeclarations requiredPrimitives =
      joinS ", " $
        ["exit"] ++
        orEmpty ("putChar" `elem` requiredPrimitives) ["putchar"] ++
        orEmpty ("putNum" `elem` requiredPrimitives) ["printf"]

compileFunction :: FunctionT -> M [Asm]
compileFunction (Function name retType params body) = do
  enterFunction paramNames localNames
  cBody <- compileBlock body
  numTemporaries <- getCurrentFunctionTemporaries
  let numLocals = fromIntegral (length localNames) + numTemporaries
   in
     return $ [
           "cuca_" ++ name ++ ":",
           "    push rbp",
           "    mov rbp, rsp"
       ] ++
       orEmpty (numLocals > 0)
         [
             "    sub rsp, " ++ show (8 * numLocals)
         ] ++
       cBody ++
       --orEmpty (numLocals > 0)
       --  [
       --      "    add rsp, " ++ show (8 * numLocals)
       --  ] ++
       [
           "    mov rsp, rbp",
           "    pop rbp",
           "    ret"
       ]
  where paramNames = map parameterName params
        localNames = freeVariables body \\ paramNames

compileBlock :: BlockT -> M [Asm]
compileBlock (Block statements) =
  concat <$> mapM compileStmt statements

getVariableAddress :: Id -> M String
getVariableAddress id = do
  parameters <- getCurrentFunctionParameters
  locals <- getCurrentFunctionLocals
  case elemIndex id parameters of
    Just i ->
      return $ "[rbp + " ++ show ((i + 2) * 8) ++ "]"
    Nothing ->
      case elemIndex id locals of
        Just i ->
          return $ "[rbp - " ++ show ((i + 1) * 8) ++ "]"
        Nothing -> error ("Variable " ++ id ++ " is not parameter or local.")

compileStmt :: StmtT -> M [Asm]
compileStmt (StmtAssign x expr) = do
  addr <- getVariableAddress x
  cExpr <- compileExpr allAvailablePlaces expr
  return $
    cExpr ++
    [
      "    mov " ++ addr ++ ", " ++ show (head allAvailablePlaces)
    ]
compileStmt (StmtCall "putChar" [expr]) = do
  requirePrimitive "putChar"
  cExpr <- compileExpr (putInFront RDI allAvailablePlaces) expr
  return $
    cExpr ++
    [
        "    call putchar"
    ]
compileStmt (StmtCall "putNum" [expr]) = do
  requirePrimitive "putNum"
  cExpr <- compileExpr (putInFront RSI allAvailablePlaces) expr
  return $
    cExpr ++
    [
        "    mov " ++ show RDI ++ ", lli_format_string",
        "    mov " ++ show RAX ++ ", 0",
        "    call printf"
    ]
compileStmt (StmtCall f exprs) = do
  cArgs <- mapM (uncurry compileArgument) (zip exprs [0..])
  return $
    orEmpty (nonempty exprs)
      [
        "    sub rsp, " ++ show (8 * length exprs)
      ] ++
    concat cArgs ++
    [
        "    call cuca_" ++ f
    ] ++
    orEmpty (nonempty exprs)
      [
        "    add rsp, " ++ show (8 * length exprs)
      ]
  where
    compileArgument :: ExprT -> Integer -> M [Asm]
    compileArgument expr i = do
      cExpr <- compileExpr allAvailablePlaces expr
      return $
        cExpr ++
        [
            "    mov [rsp + " ++ show (i * 8) ++ "], " ++
            show (head allAvailablePlaces)
        ]
compileStmt (StmtReturn expr) =
    compileExpr (RAX : allAvailablePlaces) expr
compileStmt (StmtIf expr block) = do
  cExpr <- compileExpr allAvailablePlaces expr
  labelEnd <- makeLabel
  cBlock <- compileBlock block
  return $
    cExpr ++
    [
        "    cmp " ++ show (head allAvailablePlaces) ++ ", 0",
        "    je  " ++ labelEnd
    ] ++
    cBlock ++
    [
        labelEnd ++ ":"
    ]
compileStmt (StmtIfElse expr blockThen blockElse) = do
  cExpr <- compileExpr allAvailablePlaces expr
  labelElse <- makeLabel
  labelEnd <- makeLabel
  cBlockThen <- compileBlock blockThen
  cBlockElse <- compileBlock blockElse
  return $
    cExpr ++
    [
        "    cmp " ++ show (head allAvailablePlaces) ++ ", 0",
        "    je  " ++ labelElse
    ] ++
    cBlockThen ++
    [
        "    jmp " ++ labelEnd,
        labelElse ++ ":"
    ] ++
    cBlockElse ++
    [
        labelEnd ++ ":"
    ]
compileStmt (StmtWhile expr block) = do
  cExpr <- compileExpr allAvailablePlaces expr
  labelStart <- makeLabel
  labelEnd <- makeLabel
  cBlock <- compileBlock block
  return $
    [
        labelStart ++ ":"
    ] ++
    cExpr ++
    [
        "    cmp " ++ show (head allAvailablePlaces) ++ ", 0",
        "    je  " ++ labelEnd
    ] ++
    cBlock ++
    [
        "    jmp " ++ labelStart,
        labelEnd ++ ":"
    ]
compileStmt (StmtVecAssign id expr exprValue) = do
  cExpr <- compileExpr allAvailablePlaces expr
  cExprValue <- compileExpr (tail allAvailablePlaces) exprValue
  r1 <- getPlace (head allAvailablePlaces)
  addr <- getVariableAddress id
  return $
    cExpr ++
    cExprValue ++
    [
        "    mov " ++ show RAX ++ ", " ++ r1,
        "    inc " ++ show RAX,
        "    sal " ++ show RAX ++ ", 3",
        "    add " ++ show RAX ++ ", " ++ addr,
        "    mov [rax], " ++ show (head (tail allAvailablePlaces))
    ]

getPlace :: Place -> M String
getPlace (Temporary k) = do
  updateCurrentFunctionTemporaries k  
  l <- fromIntegral . length <$> getCurrentFunctionLocals
  return $ "[rbp - " ++ show ((l + k) * 8) ++ "]"
getPlace register = return $ show register

operationToPlace :: String -> Place -> String -> M [Asm]
operationToPlace operation place value = do
  r <- getPlace place
  case place of
    Temporary _ ->
      return $
        [
          "    mov " ++ show scratch ++ ", " ++ value,
          "    " ++ operation ++ " " ++ r ++ ", " ++ show scratch
        ]
    _ ->
      return $
        [
          "    " ++ operation ++ " " ++ r ++ ", " ++ value
        ]

movToPlace :: Place -> String -> M [Asm]
movToPlace place value = operationToPlace "mov" place value

addToPlace :: Place -> String -> M [Asm]
addToPlace place value = operationToPlace "add" place value

subToPlace :: Place -> String -> M [Asm]
subToPlace place value = operationToPlace "sub" place value

andToPlace :: Place -> String -> M [Asm]
andToPlace place value = operationToPlace "and" place value

orToPlace :: Place -> String -> M [Asm]
orToPlace place value = operationToPlace "or" place value

cmpToPlace :: Place -> String -> M [Asm]
cmpToPlace place value = operationToPlace "cmp" place value

compileRelationalExpression :: String -> [Place] -> ExprT -> M [Asm]
compileRelationalExpression condJump places (ExprEq e1 e2) = do
  r2 <- getPlace (places !! 1)
  c1 <- compileExpr places e1
  c2 <- compileExpr (tail places) e2
  cOp <- cmpToPlace (places !! 0) r2 
  cMovFalse <- movToPlace (places !! 0) "0"
  cMovTrue  <- movToPlace (places !! 0) "-1"
  l1 <- makeLabel
  l2 <- makeLabel
  return $ c1 ++
           c2 ++
           cOp ++
           [
               "    " ++ condJump ++ " " ++ l1
           ] ++
           cMovFalse ++
           [
               "    jmp " ++ l2,
               l1 ++ ":"
           ] ++
           cMovTrue ++
           [
               l2 ++ ":"
           ]

movIndirect :: Place -> Integer -> String -> M [Asm]
movIndirect place@(Temporary _) k s = do
  r1 <- getPlace place
  m <- movIndirect scratch k s
  return $
    [
      "    mov " ++ show scratch ++ ", " ++ r1
    ] ++
    m
movIndirect place 0 s =
  return $
    [
        "    mov qword [" ++ show place ++ "], " ++ s
    ]
movIndirect place k s =
  return $
    [
        "    mov qword [" ++ show place ++ " + " ++ show (8 * k) ++ "], " ++ s
    ]

movIndirectPlace :: Place -> Integer -> Place -> M [Asm]
movIndirectPlace place1 k place2@(Temporary _) = do
  r2 <- getPlace place2
  m <- movIndirect place1 k (show RAX)
  return $
    [
        "    mov " ++ show RAX ++ ", " ++ r2
    ] ++
    m
movIndirectPlace place1 k place2 =
  movIndirect place1 k (show place2)

compileExpr :: [Place] -> ExprT -> M [Asm]
compileExpr places (ExprVar id) = do
  addr <- getVariableAddress id
  movToPlace (head places) addr
compileExpr places (ExprConstNum n) = do
  movToPlace (head places) (show n)
compileExpr places (ExprConstBool False) = do
  movToPlace (head places) "0"
compileExpr places (ExprConstBool True) = do
  movToPlace (head places) "-1"

compileExpr places (ExprAnd e1 e2) = do
  r2 <- getPlace (places !! 1)
  c1 <- compileExpr places e1
  c2 <- compileExpr (tail places) e2
  cOp <- andToPlace (places !! 0) r2 
  return $ c1 ++ c2 ++ cOp
compileExpr places (ExprOr e1 e2) = do
  r2 <- getPlace (places !! 1)
  c1 <- compileExpr places e1
  c2 <- compileExpr (tail places) e2
  cOp <- orToPlace (places !! 0) r2 
  return $ c1 ++ c2 ++ cOp
compileExpr places (ExprNot e) = do
  r <- getPlace (places !! 0)
  c <- compileExpr places e
  return $ c ++
           [
               "    not " ++ r
           ]
compileExpr places (ExprLe e1 e2) =
  compileRelationalExpression "jle" places (ExprEq e1 e2)
compileExpr places (ExprGe e1 e2) =
  compileRelationalExpression "jge" places (ExprEq e1 e2)
compileExpr places (ExprLt e1 e2) =
  compileRelationalExpression "jl" places (ExprEq e1 e2)
compileExpr places (ExprGt e1 e2) =
  compileRelationalExpression "jg" places (ExprEq e1 e2)
compileExpr places (ExprEq e1 e2) =
  compileRelationalExpression "je" places (ExprEq e1 e2)
compileExpr places (ExprNe e1 e2) =
  compileRelationalExpression "jne" places (ExprEq e1 e2)
compileExpr places (ExprAdd e1 e2) = do
  r2 <- getPlace (places !! 1)
  c1 <- compileExpr places e1
  c2 <- compileExpr (tail places) e2
  cOp <- addToPlace (places !! 0) r2 
  return $ c1 ++ c2 ++ cOp
compileExpr places (ExprSub e1 e2) = do
  r2 <- getPlace (places !! 1)
  c1 <- compileExpr places e1
  c2 <- compileExpr (tail places) e2
  cOp <- subToPlace (places !! 0) r2 
  return $ c1 ++ c2 ++ cOp
compileExpr places (ExprMul e1 e2) = do
  r1 <- getPlace (places !! 0)
  r2 <- getPlace (places !! 1)
  c1 <- compileExpr places e1
  c2 <- compileExpr (tail places) e2
  return $
    c1 ++
    c2 ++
    [
        "    mov " ++ show RAX ++ ", " ++ r1,
        "    imul " ++ r2,
        "    mov " ++ r1 ++ ", " ++ show RAX
    ]
compileExpr places (ExprCall f exprs) = do
  cArgs <- mapM (uncurry compileArgument) (zip exprs [0..])
  r1 <- getPlace (places !! 0)
  return $
    saveCurrentlyUsedRegisters ++
    orEmpty (nonempty exprs)
      [
        "    sub rsp, " ++ show (8 * length exprs)
      ] ++
    concat cArgs ++
    [
        "    call cuca_" ++ f
     ] ++
    orEmpty (nonempty exprs)
      [
        "    add rsp, " ++ show (8 * length exprs)
      ] ++
    restoreCurrentlyUsedRegisters ++
    [
        "    mov " ++ r1 ++ ", " ++ show RAX
     ]
  where
    compileArgument :: ExprT -> Integer -> M [Asm]
    compileArgument expr i = do
      cExpr <- compileExpr places expr
      return $
        cExpr ++
        [
            "    mov [rsp + " ++ show (i * 8) ++ "], " ++
            show (head places)
        ]
    saveCurrentlyUsedRegisters :: [Asm]
    saveCurrentlyUsedRegisters =
      map (\ r -> "    push " ++ show r)
          (currentlyUsedRegisters places)
    restoreCurrentlyUsedRegisters :: [Asm]
    restoreCurrentlyUsedRegisters =
      map (\ r -> "    pop " ++ show r)
          (reverse (currentlyUsedRegisters places))
compileExpr places (ExprVecMake exprs) = do
    cExprs <- concat <$> mapM (uncurry compileEntry) (zip exprs [1..])
    let sizeInBytes = (1 + length exprs) * 8 in do
      mov_r1_rsp <- movToPlace (head places) "rsp"
      mov_r1_sub_0_len <- movIndirect (head places) 0 (show (length exprs))
      return $
        [
          "    sub rsp, " ++ show sizeInBytes
        ] ++
        mov_r1_rsp ++
        mov_r1_sub_0_len ++
        cExprs
  where
    compileEntry :: ExprT -> Integer -> M [Asm]
    compileEntry expr i = do
      cExpr <- compileExpr (tail places) expr
      m <- movIndirectPlace (head places) i (head (tail places))
      return $
        cExpr ++
        m
compileExpr places (ExprVecLength id) = do
  addr <- getVariableAddress id
  m <- movToPlace (head places) "[rax]"
  return $
    [
        "    mov " ++ show RAX ++ ", " ++ addr
    ] ++
    m
compileExpr places (ExprVecDeref id expr) = do
  cExpr <- compileExpr places expr
  r1 <- getPlace (head places)
  addr <- getVariableAddress id
  m <- movToPlace (head places) "[rax]"
  return $
    cExpr ++
    [
        "    mov " ++ show RAX ++ ", " ++ r1,
        "    inc " ++ show RAX,
        "    sal " ++ show RAX ++ ", 3",
        "    add " ++ show RAX ++ ", " ++ addr
    ] ++
    m

