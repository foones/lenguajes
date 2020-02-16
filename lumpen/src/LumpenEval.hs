module LumpenEval(eval) where

import Lumpen

-- utility to join strings
joinS :: [a] -> [[a]] -> [a]
joinS sep []     = []
joinS sep [x]    = x
joinS sep (x:xs) = x ++ sep ++ joinS sep xs
--

-- Semantics --

type Env = Id -> Value

emptyEnv :: Env
emptyEnv x = ErrV ("unbound variable " ++ show x)

extendEnv :: Id -> Value -> Env -> Env
extendEnv x v env y
  | x == y    = v
  | otherwise = env y

type Ref = Int
data Store = Store Int (Ref -> Value)

emptyStore :: Store
emptyStore = Store 0 . const . ErrV $ "undefined reference"

allocStore :: Store -> Value -> (Store, Value)
allocStore (Store l f) v = (Store (l + 1) g, RefV l)
  where g r | r == l    = v
            | otherwise = f r

derefStore :: Store -> Ref -> Value
derefStore (Store _ f) = f

assignStore :: Store -> Ref -> Value -> Store
assignStore (Store l f) r0 v = Store l g
  where g r | r == r0   = v
            | otherwise = f r

data Value = ConstV Constant
           | FunV (Value -> Store -> Cont -> Result)
           | TupleV [Value]
           | ListV [Value]
           | RefV Ref
           | ErrV String

unitV = TupleV []

instance Show Value where
  show (ConstV c)        = show c
  show (FunV _)          = "<function>"
  show (TupleV vs)       = "(" ++ joinS ", " (map show vs) ++ ")"
  show (ListV vs)        = "[" ++ joinS ", " (map show vs) ++ "]"
  show (RefV ref)        = "<ref " ++ show ref ++ ">"
  show (ErrV msg)        = "<error: " ++ msg ++ ">"

type Cont = (Store, Value) -> Result

sem :: Expr -> Env -> Store -> Cont -> Result
sem (VarE x)        e s0 k = k (s0, e x)
sem (LamE x a)      e s0 k = k (s0, FunV (\ v s1 k' -> sem a (extendEnv x v e) s1 k'))
sem (AppE a b)      e s0 k = sem a e s0 (\ (s1, va) ->
                             sem b e s1 (\ (s2, vb) ->
                               case va of
                                 FunV f -> f vb s2 k
                                 _      -> k (s2, ErrV "application requires a function")))
sem (ConstE c)      _ s0 k = k (s0, ConstV c)
sem (IfE a b c)     e s0 k = sem a e s0 (\ (s1, va) ->
                               case va of
                                 ConstV (BoolC False) -> sem c e s1 k
                                 _                    -> sem b e s1 k)
-- tuples
sem (TupleE [])     _ s0 k = k (s0, TupleV [])
sem (TupleE (a:as)) e s0 k = sem a e s0 (\ (s1, va) ->
                             sem (TupleE as) e s1 (\ (s2, TupleV vas) ->
                             k (s2, TupleV (va:vas))))
-- lists
sem (ListE [])     _ s0 k = k (s0, ListV [])
sem (ListE (a:as)) e s0 k = sem a e s0 (\ (s1, va) ->
                            sem (ListE as) e s1 (\ (s2, ListV vas) ->
                            k (s2, ListV (va:vas))))
-- call/cc
sem (CallccE a)     e s0 k = sem a e s0 (\ (s1, va) ->
                               case va of
                                 FunV f -> f (FunV (\ v s k' -> k (s, v))) s1 k
                                 _      -> k (s1, ErrV "call/cc argument should be a function"))
-- references
sem (UnaryE NewOp a)    e s0 k = sem a e s0 (\ (s1, va) ->
                                   k (allocStore s1 va))
sem (UnaryE DerefOp a)  e s0 k = sem a e s0 (\ (s1, va) ->
                                   case va of
                                     RefV r -> k (s1, derefStore s1 r)
                                     _      -> k (s1, ErrV "dereferencing not a reference"))
sem (BinopE AssOp a b) e s0 k = sem a e s0 (\ (s1, va) ->
                                sem b e s1 (\ (s2, vb) ->
                                   case va of
                                     RefV r -> k (assignStore s1 r vb, unitV)
                                     _      -> k (s1, ErrV "dereferencing not a reference")))
sem (BinopE SeqOp a b) e s0 k = sem a e s0 (\ (s1, _) ->
                                sem b e s1 (\ (s2, vb) ->
                                  k (s2, vb)))
-- short-circuiting and / or
sem (BinopE AndOp a b) e s0 k = sem a e s0 (\ (s1, va) ->
                                case va of
                                  ConstV (BoolC False) -> k (s1, ConstV (BoolC False))
                                  _                    -> sem b e s1 k)
sem (BinopE OrOp a b) e s0 k  = sem a e s0 (\ (s1, va) ->
                                case va of
                                  ConstV (BoolC True)  -> k (s1, ConstV (BoolC True))
                                  _                    -> sem b e s1 k)
-- generic binary and unary operators
sem (BinopE op a b) e s0 k = sem a e s0 (\ (s1, va) ->
                             sem b e s1 (\ (s2, vb) ->
                             k (s2, sembinop op va vb)))
sem (UnaryE op a)   e s0 k = sem a e s0 (\ (s1, va) ->
                             k (s1, semunop op va))

sembinop :: Op -> Value -> Value -> Value
sembinop ConsOp = consVal
sembinop EqOp   = liftNumBinop boolVal (==)
sembinop NeOp   = liftNumBinop boolVal (/=)
sembinop LtOp   = liftNumBinop boolVal (<)
sembinop LeOp   = liftNumBinop boolVal (<=)
sembinop GtOp   = liftNumBinop boolVal (>)
sembinop GeOp   = liftNumBinop boolVal (>=)
sembinop AddOp  = liftNumBinop numVal  (+)
sembinop SubOp  = liftNumBinop numVal  (-)
sembinop MulOp  = liftNumBinop numVal  (*)
sembinop DivOp  = liftNumBinop numVal  div
sembinop ModOp  = liftNumBinop numVal  mod
sembinop PowOp  = liftNumBinop numVal  (^)

consVal :: Value -> Value -> Value
consVal v (ListV vs) = ListV (v : vs)
consVal _ _          = ErrV "cons should take a list"

semunop :: Op -> Value -> Value
semunop NotOp   = liftBoolUnop boolVal not
semunop NegOp   = liftNumUnop numVal negate

liftNumBinop :: (a -> Value) -> (Integer -> Integer -> a) -> Value -> Value -> Value
liftNumBinop f op (ConstV (IntC n)) (ConstV (IntC m)) = f (op n m)
liftNumBinop _ _ _ _ = ErrV "numeric operator applied to non-numbers"

liftBoolUnop :: (a -> Value) -> (Bool -> a) -> Value -> Value
liftBoolUnop f op (ConstV (BoolC b)) = f (op b)
liftBoolUnop _ _ _ = ErrV "boolean operator applied to non-boolean"

liftNumUnop :: (a -> Value) -> (Integer -> a) -> Value -> Value
liftNumUnop f op (ConstV (IntC n)) = f (op n)
liftNumUnop _ _ _ = ErrV "numeric operator applied to non-number"

boolVal :: Bool -> Value
boolVal = ConstV . BoolC

numVal :: Integer -> Value
numVal = ConstV . IntC

type Result = IO ()

fixpoint :: Value
fixpoint = FunV (\ v s0 k ->
                   sem ycombinator emptyEnv s0 (\ (s1, fix) ->
                     case fix of
                       FunV f -> f v s1 k
                       _      -> error "internal error, fixpoint should be a function"))
  where ycombinator = AppE w w
        w = LamE "x"
              (LamE "f"
                (AppE
                  (VarE "f")
                  (LamE "y"
                    (AppE
                      (AppE
                        (AppE (VarE "x") (VarE "x"))
                        (VarE "f"))
                      (VarE "y")))))

liftFun :: (Value -> Value) -> Value
liftFun f = FunV (\v s0 k -> k (s0, f v))

fstVal :: Value
fstVal = liftFun f
  where f (TupleV (x:_:[])) = x
        f _                 = ErrV "argument of fst must be a pair"

sndVal :: Value
sndVal = liftFun f
  where f (TupleV (_:y:[])) = y
        f _                 = ErrV "argument of snd must be a pair"

hdVal :: Value
hdVal = liftFun f
  where f (ListV (v:_)) = v
        f _             = ErrV "argument of hd should be a non-empty list"

tlVal :: Value
tlVal = liftFun f
  where f (ListV (_:vs)) = ListV vs
        f _              = ErrV "argument of tl should be a non-empty list"

nullVal :: Value
nullVal = liftFun f
  where f (ListV []) = ConstV $ BoolC True
        f _          = ConstV $ BoolC False

printVal :: Value
printVal = FunV (\ v s0 k ->
                   do putStr (show v);
                      k (s0, unitV))

printlnVal :: Value
printlnVal = FunV (\ v s0 k ->
                   do putStr (show v ++ "\n");
                      k (s0, unitV))

globalEnv :: Env
globalEnv = foldr (uncurry extendEnv) emptyEnv [
              ("%fix%", fixpoint),
              ("fst", fstVal),
              ("snd", sndVal),
              ("null", nullVal),
              ("hd", hdVal),
              ("tl", tlVal),
              ("print", printVal),
              ("println", printlnVal)
            ]

eval :: Expr -> Result
eval x = sem x globalEnv emptyStore (\ (_, v) -> print v)

