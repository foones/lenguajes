module Lang(Id, Term(..), Program, Statement(..), M(..), Env, checkProgram) where

import Data.Maybe
import Data.Char
import Data.List
import Control.Applicative
import Control.Monad

import Lexer

data Term = Var Id 
          | Lam Id Term Term
          | App Term Term

instance Show Term where
    show = joinToks . snd . rec
      where rec (Var x) = (lAtom, [x])
            rec (Lam x typ term)
              | x `notElem` vs = (lExp, [">"] ++
                                        paren lAtom (rec typ) ++
                                        paren lExp (rec term))
              | otherwise = (lExp, [":", x] ++
                                   paren lAppExp (rec typ) ++
                                   ["."] ++
                                   paren lExp (rec term))
              where vs = freeVars =<< [typ, term]
            rec (App a b) = (lAppExp, paren lAppExp (rec a) ++ paren lAtom (rec b))
            paren k1 (k2, x)
              | k1 < k2   = ["("] ++ x ++ [")"]
              | otherwise = x
            lExp    = 3
            lAppExp = 2
            lAtom   = 1
            joinToks []           = []
            joinToks [v]          = v
            joinToks (".":":":ws) = "," ++ joinToks ws
            joinToks (v:w:ws)     = v ++ sep ++ joinToks (w:ws)
              where
                sep
                  | isLongIdent v && isIdent w      = " "
                  | isIdent v && isNumIdent w       = " "
                  | otherwise                       = ""

type Program = [Statement]

data Statement = Assume Id Term
               | Prove Id Term Term
               | Define Id Term
               | AskType Term
               | AnswerType Term Term
               | AskValue Term
               | AnswerValue Term Term Term

instance Show Statement where
    show (Assume x a)        = x ++ " : " ++ show a ++ "."
    show (Prove x a b)       = x ++ " : " ++ show a ++ " = " ++ show b ++ "."
    show (Define x a)        = x ++ " = " ++ show a ++ "."
    show (AskType a)         = "? " ++ show a ++ "."
    show (AnswerType a b)    = "! " ++ show a ++ " : " ++ show b ++ "."
    show (AskValue a)        = "?? " ++ show a ++ "."
    show (AnswerValue a b c) = "!! " ++ show a ++ " : " ++ show b ++ " = " ++ show c ++ "."

data Env = Env [EnvFact]

joinSep :: String -> [String] -> String
joinSep sep []       = ""
joinSep sep [x]      = x
joinSep sep (x:y:xs) = x ++ sep ++ joinSep sep (y:xs)

instance Show Env where
    show (Env env) = "{" ++ (joinSep " ; " . map show $ env) ++ "}"

data EnvFact = EnvType Id Term
             | EnvValue Id Term Term

instance Show EnvFact where
    show (EnvType x a)    = x ++ " : " ++ show a
    show (EnvValue x a b) = x ++ " : " ++ show a ++ " = " ++ show b

data M a = Error String
         | OK a
         deriving Show

instance Functor M where
    fmap _ (Error msg) = Error msg
    fmap f (OK x)      = OK (f x)

instance Applicative M where
    pure                    = OK
    Error msg <*> _         = Error msg
    OK   _    <*> Error msg = Error msg
    OK f      <*> OK x      = OK (f x)

instance Monad M where
    fail   = Error
    return = OK
    x >>= f  = case x of
                 Error msg  -> Error msg
                 OK val -> f val

applyJust :: (a -> Maybe a) -> [a] -> Maybe [a]
applyJust f []     = Nothing
applyJust f (x:xs) = maybe (return . (x:) =<< applyJust f xs) (return . (:xs)) (f x)

freeVars :: Term -> [Id]
freeVars (Var x)       = [x]
freeVars (App a b)     = freeVars a `union` freeVars b
freeVars (Lam x typ a) = freeVars typ `union` (freeVars a \\ [x])

freshId :: Id -> [Term] -> Id
freshId x as = head . dropWhile (`elem` vs) . map (x ++) $ suffixes
    where suffixes = [""] ++ map show [1..]
          vs       = freeVars =<< as

substitute :: Term -> Id -> Term -> Term
substitute (Var x) y s
    | x == y = s
    | x /= y = Var x
substitute (App a b) y s = App (substitute a y s) (substitute b y s)
substitute (Lam x typ a) y s
    | x == y = Lam x typ a
    | otherwise = Lam z (sub typ) (sub a)
    where z = freshId x [s]
          sub t = substitute (substitute t x (Var z)) y s

renameBindings :: (Id, Term, Term) -> (Id, Term, Term) -> (Term, Term)
renameBindings (x, s, a) (y, t, b) = (a', b')
  where a' = substitute a x z
        b' = substitute b y z
        z  = Var $ freshId x [a, b, s, t]

renameAbstraction :: Env -> (Id, Term, Term) -> (Id, Term, Term)
renameAbstraction env (x, s, a) = (x', s, a')
  where a' = substitute a x (Var x')
        x' = freshId x ([s, a] ++ envTerms env)

alphaEqual :: Term -> Term -> Bool
alphaEqual (Var x)     (Var y)     = x == y
alphaEqual (App a1 b1) (App a2 b2) = alphaEqual a1 a2 && alphaEqual b1 b2
alphaEqual (Lam x s a) (Lam y t b) = alphaEqual s t && alphaEqual a' b'
  where (a', b') = renameBindings (x, s, a) (y, t, b)
alphaEqual _ _                     = False

---- Reduction

reduceInEnv1 :: Env -> Term -> Maybe Term
reduceInEnv1 env (Var x)               = lookupEnvValue x env
reduceInEnv1 env (Lam x typ body)      = return . Lam x' typ' =<< reduceInEnv1 env body'
  where (x', typ', body') = renameAbstraction env (x, typ, body)
reduceInEnv1 env (App (Lam x typ a) b) = return (substitute a x b)
-- delta rules
reduceInEnv1 env (App (App (App (Var "~") _) x) y)
  | betaEqualInEnv env x y             = return (Var "True")
  | otherwise                          = return (Var "False")
--
reduceInEnv1 env (App a b)             = return . lApp =<< applyJust (reduceInEnv1 env) [a, b]
  where lApp [x, y] = App x y

reduceInEnv :: Env -> Term -> Term
reduceInEnv env = fromJust . last . takeWhile p . iterate (>>= reduceInEnv1 env) . return
  where p = maybe False (const True)

betaEqualInEnv :: Env -> Term -> Term -> Bool
betaEqualInEnv env term1 term2 = alphaEqual n1 n2
  where n1 = reduceInEnv env term1
        n2 = reduceInEnv env term2

-- Leq comparisons return True if term1 is *more generic* than term2
alphaStarLeq :: Term -> Term -> Bool
alphaStarLeq (Var "*") b = isStarClosure b
  where isStarClosure (Var "*")   = True
        isStarClosure (Var _)     = False
        isStarClosure (App _ _)   = False
        isStarClosure (Lam _ _ b) = isStarClosure b
alphaStarLeq (App a1 b1) (App a2 b2) = alphaEqual a1 a2 && alphaEqual b1 b2
alphaStarLeq (Lam x s a) (Lam y t b) = alphaEqual s t && alphaStarLeq a' b'
  where (a', b') = renameBindings (x, s, a) (y, t, b)
alphaStarLeq _ _                       = False

betaLeqInEnv :: Env -> Term -> Term -> Bool
betaLeqInEnv env term1 term2 = alphaEqual n1 n2 || alphaStarLeq n1 n2
  where n1 = reduceInEnv env term1
        n2 = reduceInEnv env term2

----

emptyEnv :: Env
emptyEnv = Env []

globalEnv :: Env
globalEnv = foldr (uncurry extendEnvType) emptyEnv [
                ("*", star),
                ("~", Lam "a" (Var "*") (Lam "x" (Var "a") (Lam "y" (Var "a") (Var "Prop"))))
            ]

extendEnvType :: Id -> Term -> Env -> Env
extendEnvType x typ (Env env) = Env (EnvType x typ:env)

extendEnvValue :: Id -> Term -> Term -> Env -> Env
extendEnvValue x trm typ (Env env) = Env (EnvValue x trm typ:env)

envVars :: EnvFact -> [(Id, Term)]
envVars (EnvType x _)    = [(x, Var x)]
envVars (EnvValue x _ _) = [(x, Var x)]

envType :: EnvFact -> [(Id, Term)]
envType (EnvType x s)    = [(x, s)]
envType (EnvValue x _ s) = [(x, s)]

envValue :: EnvFact -> [(Id, Term)]
envValue (EnvValue x v _) = [(x, v)]
envValue _                = []

collectTerms :: Env -> (EnvFact -> [(Id, Term)]) -> [Term]
collectTerms (Env env) f = map snd (f =<< env)

lookupEnvType :: Id -> Env -> Maybe Term
lookupEnvType x (Env env) = lookup x (concatMap envType env)

lookupEnvValue :: Id -> Env -> Maybe Term
lookupEnvValue x (Env env) = lookup x (concatMap envValue env)

envTerms :: Env -> [Term]
envTerms env = collectTerms env =<< [envVars, envType, envValue]

----

star :: Term
star = Var "*"

typecheck :: Env -> Term -> Term -> M Term
typecheck env inferTerm typ = do
    typ' <- typeinfer env inferTerm
    if betaLeqInEnv env typ typ'
     then return typ'
     else fail ("types do not match: " ++ show typ ++ " -- " ++ show typ' ++ " in env " ++ show env)

typeinfer :: Env -> Term -> M Term
typeinfer env (Var x)   = maybe (fail $ "unbound variable " ++ x) return (lookupEnvType x env)
typeinfer env (App a b) = do
    a' <- typeinfer env a
    case reduceInEnv env a' of
        Lam aVar aType aBody -> do
            case typecheck env b aType of
                OK _      -> return (substitute aBody aVar b)
                Error msg -> fail ("in application " ++ show (App a b) ++ " -- argument has wrong type\n" ++ msg)
        _ -> fail ("in application " ++ show (App a b) ++ " -- function is not an abstraction, its type is " ++ show a' ++ "\n")
typeinfer env (Lam x s a) = do
    typeinfer env s
    b <- typeinfer (extendEnvType z s env) a'
    return $ Lam z s b
  where
    z  = freshId x (envTerms env)
    a' = substitute a x (Var z)

checkNotInEnv :: Id -> Env -> M ()
checkNotInEnv x env = maybe (return ()) (const f) $ lookupEnvType x env
  where f = fail ("identifier " ++ x ++ " defined twice in environment")

addRet :: Statement -> (Env, Program) -> M (Env, Program)
addRet s (e, p) = return (e, s : p)

checkProgramInEnv :: Env -> Program -> M (Env, Program)
-- Statements
checkProgramInEnv env [] = OK (env, [])
checkProgramInEnv env p@(Assume x typ:prog) = do
    checkNotInEnv x env
    typeinfer env typ
    res <- checkProgramInEnv (extendEnvType x typ env) prog
    addRet (head p) res
checkProgramInEnv env p@(Prove x typ term:prog) = do
    checkNotInEnv x env
    typeinfer env typ
    typecheck env term typ
    res <- checkProgramInEnv (extendEnvValue x term typ env) prog
    addRet (head p) res
checkProgramInEnv env p@(Define x term:prog) = do
    checkNotInEnv x env
    typ <- typeinfer env term
    res <- checkProgramInEnv (extendEnvValue x term typ env) prog
    addRet (head p) res
-- Queries
checkProgramInEnv env (AskType term:prog) = do
    typ <- typeinfer env term
    res <- checkProgramInEnv env prog
    addRet (AnswerType term typ) res
checkProgramInEnv env (AskValue term:prog) = do
    typ <- typeinfer env term
    res <- checkProgramInEnv env prog
    addRet (AnswerValue term typ (reduceInEnv env term)) res

checkProgram :: Maybe Env -> Program -> M (Env, Program)
checkProgram mEnv = checkProgramInEnv env
    where env = maybe globalEnv id mEnv

