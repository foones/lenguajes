
module Syntax.AST(
         AProgram(..), ADeclaration(..), AParameter(..),
         AConstructorDecl(..),
         AExpr(..), appMany, appManyOpt, lamManyParams,
         piManyParams, makeImplicit, patternHead, parameterName,
         AOptionalExpr(..),
         ACaseBranch(..), APattern(..),
         Program, Declaration, Parameter, ConstructorDecl,
         Expr, OptionalExpr, CaseBranch, Pattern,
         annotation, eraseAnnotations, exprToPattern, patternToExpr
       ) where

import Utils(joinS)
import qualified Syntax.Position as Pos
import Syntax.Name(QName, HoleName)

data AProgram a = Program [ADeclaration a]
  deriving Eq

data ADeclaration a =
    DeclDataType a QName [AParameter a] (AExpr a)
  | DeclDataConstructors a QName [AParameter a] [AConstructorDecl a]
  | DeclType a QName (AExpr a)
  | DeclEquation a (APattern a) (AExpr a)
  | DeclCheck a (AExpr a)
  deriving Eq

data AParameter a = ExplicitParameter a QName (AExpr a)
                  | ImplicitParameter a QName (AExpr a)
  deriving Eq

parameterName :: AParameter a -> QName
parameterName (ExplicitParameter _ name _) = name
parameterName (ImplicitParameter _ name _) = name

makeImplicit :: AParameter a -> AParameter a
makeImplicit (ExplicitParameter ann x t) = ImplicitParameter ann x t
makeImplicit (ImplicitParameter ann x t) = ImplicitParameter ann x t

data AConstructorDecl a =
    ConstructorDecl a QName (AExpr a)
  deriving Eq

data AExpr a =
    EWildcard a
  | EHole a HoleName
  | EVar a QName
  | ELam a QName (AExpr a) (AExpr a)
  | ELamImplicit a QName (AExpr a) (AExpr a)
  | EApp a (AExpr a) (AExpr a)
  | EAppImplicit a (AExpr a) (AExpr a)
  | ECase a (AExpr a) [ACaseBranch a]
  | EPi a (AParameter a) (AExpr a)
  | ESig a (AParameter a) (AExpr a)
  | EInaccessible a (AExpr a) -- for inaccessible ("dotted") patterns
  deriving Eq

appMany :: AExpr a -> [AExpr a] -> AExpr a
appMany fun args = foldl1 (\ acc e -> EApp (annotation e) acc e) (fun : args)

appManyOpt :: AExpr a -> [AOptionalExpr a] -> AExpr a
appManyOpt fun [] = fun
appManyOpt fun (OptPlain expr : optExprs) =
  appManyOpt (EApp (annotation fun) fun expr) optExprs
appManyOpt fun (OptOptional expr : optExprs) =
  appManyOpt (EAppImplicit (annotation fun) fun expr) optExprs

lamManyParams :: [AParameter a] -> AExpr a -> AExpr a
lamManyParams [] body = body
lamManyParams (ExplicitParameter ann qname typ : params) body =
  ELam ann qname typ (lamManyParams params body)
lamManyParams (ImplicitParameter ann qname typ : params) body =
  ELamImplicit ann qname typ (lamManyParams params body)

piManyParams :: [AParameter a] -> AExpr a -> AExpr a
piManyParams [] body = body
piManyParams (param : params) body = EPi (annotation param) param
                                       (piManyParams params body)

patternHead :: APattern a -> APattern a
patternHead (PApp _ fun _)         = patternHead fun
patternHead (PAppImplicit _ fun _) = patternHead fun
patternHead pat                    = pat

data AOptionalExpr a = OptPlain (AExpr a)
                     | OptOptional (AExpr a)

data ACaseBranch a =
    CaseBranch a [APattern a] (AExpr a)
  deriving Eq

data APattern a =
    PWildcard a
  | PHole a HoleName
  | PVar a QName
  | PApp a (APattern a) (APattern a)
  | PAppImplicit a (APattern a) (APattern a)
  | PInaccessible a (APattern a)
  deriving Eq

----

class Annotation c where
  annotation :: c a -> a

instance Annotation ADeclaration where
  annotation (DeclDataType ann _ _ _)         = ann
  annotation (DeclDataConstructors ann _ _ _) = ann
  annotation (DeclType ann _ _)               = ann
  annotation (DeclEquation ann _ _)           = ann
  annotation (DeclCheck ann _)                = ann

instance Annotation AExpr where
  annotation (EWildcard a)          = a
  annotation (EHole a _)            = a
  annotation (EVar a _)             = a
  annotation (ELam a _ _ _)         = a
  annotation (ELamImplicit a _ _ _) = a
  annotation (EApp a _ _)           = a
  annotation (EAppImplicit a _ _)   = a
  annotation (ECase a _ _)          = a
  annotation (EPi a _ _)            = a
  annotation (ESig a _ _)           = a
  annotation (EInaccessible a _)    = a

instance Annotation AParameter where
  annotation (ExplicitParameter ann _ _) = ann
  annotation (ImplicitParameter ann _ _) = ann

instance Annotation APattern where
  annotation (PWildcard a)          = a
  annotation (PHole a _)            = a
  annotation (PVar a _)             = a
  annotation (PApp a _ _)           = a
  annotation (PAppImplicit a _ _)   = a
  annotation (PInaccessible a _)    = a

----

type Program         = AProgram Pos.Position
type Declaration     = ADeclaration Pos.Position
type Parameter       = AParameter Pos.Position
type ConstructorDecl = AConstructorDecl Pos.Position
type Expr            = AExpr Pos.Position
type OptionalExpr    = AOptionalExpr Pos.Position
type CaseBranch      = ACaseBranch Pos.Position
type Pattern         = APattern Pos.Position

----

class EraseAnnotations c where
  eraseAnnotations :: c a -> c ()

instance EraseAnnotations AProgram where
  eraseAnnotations (Program xs) = Program (map eraseAnnotations xs)

instance EraseAnnotations ADeclaration where
  eraseAnnotations (DeclDataType _ qname params expr) =
    DeclDataType () qname (map eraseAnnotations params)
                          (eraseAnnotations expr)
  eraseAnnotations (DeclDataConstructors _ qname params cdecls) =
    DeclDataConstructors () qname (map eraseAnnotations params)
                                  (map eraseAnnotations cdecls)
  eraseAnnotations (DeclType _ qname expr) =
    DeclType () qname (eraseAnnotations expr)
  eraseAnnotations (DeclEquation _ lhs rhs) =
    DeclEquation () (eraseAnnotations lhs) (eraseAnnotations rhs)
  eraseAnnotations (DeclCheck _ expr) =
    DeclCheck () (eraseAnnotations expr)

instance EraseAnnotations AParameter where
  eraseAnnotations (ExplicitParameter _ qname expr) =
    ExplicitParameter () qname (eraseAnnotations expr)
  eraseAnnotations (ImplicitParameter _ qname expr) =
    ImplicitParameter () qname (eraseAnnotations expr)

instance EraseAnnotations AConstructorDecl where
  eraseAnnotations (ConstructorDecl _ qname expr) =
    ConstructorDecl () qname (eraseAnnotations expr)

instance EraseAnnotations AExpr where
  eraseAnnotations (EWildcard _) = EWildcard ()
  eraseAnnotations (EHole _ hname) = EHole () hname
  eraseAnnotations (EVar _ qname) = EVar () qname
  eraseAnnotations (ELam _ qname expr1 expr2) =
    ELam () qname (eraseAnnotations expr1) (eraseAnnotations expr2)
  eraseAnnotations (ELamImplicit _ qname expr1 expr2) =
    ELamImplicit () qname (eraseAnnotations expr1) (eraseAnnotations expr2)
  eraseAnnotations (EApp _ expr1 expr2) =
    EApp () (eraseAnnotations expr1) (eraseAnnotations expr2)
  eraseAnnotations (EAppImplicit _ expr1 expr2) =
    EAppImplicit () (eraseAnnotations expr1) (eraseAnnotations expr2)
  eraseAnnotations (ECase _ expr branches) =
    ECase () (eraseAnnotations expr) (map eraseAnnotations branches)
  eraseAnnotations (EPi _ param expr) =
    EPi () (eraseAnnotations param) (eraseAnnotations expr)
  eraseAnnotations (ESig _ param expr) =
    ESig () (eraseAnnotations param) (eraseAnnotations expr)
  eraseAnnotations (EInaccessible _ expr) =
    EInaccessible () (eraseAnnotations expr)

instance EraseAnnotations ACaseBranch where
  eraseAnnotations (CaseBranch _ patterns expr) =
    CaseBranch () (map eraseAnnotations patterns) (eraseAnnotations expr)

instance EraseAnnotations APattern where
  eraseAnnotations (PWildcard _)        = PWildcard ()
  eraseAnnotations (PHole _ hname)      = PHole () hname
  eraseAnnotations (PVar _ qname)       = PVar () qname
  eraseAnnotations (PApp _ pat1 pat2) =
    PApp () (eraseAnnotations pat1)
            (eraseAnnotations pat2)
  eraseAnnotations (PAppImplicit _ pat1 pat2) =
    PAppImplicit () (eraseAnnotations pat1)
                    (eraseAnnotations pat2)
  eraseAnnotations (PInaccessible _ pat) =
    PInaccessible () (eraseAnnotations pat)

----

indent :: String -> String
indent x = joinS "\n" (map ("  " ++) (lines x))

instance Show a => Show (AProgram a) where
  show (Program decls) = joinS "\n" (map show decls)

instance Show a => Show (ADeclaration a) where
  show (DeclDataType _ qname params expr) =
    "data " ++ joinS " " (show qname : map show params) ++ " : " ++ show expr
  show (DeclDataConstructors _ qname params cdecls) =
    "data " ++ joinS " " (show qname : map show params) ++ " where\n" ++
    joinS "\n" (map (indent . show) cdecls)
  show (DeclType _ qname expr)  =
    show qname ++ " : " ++ show expr
  show (DeclEquation _ lhs rhs) = show lhs ++ " = " ++ show rhs
  show (DeclCheck _ expr) = "? " ++ show expr

instance Show a => Show (AParameter a) where
  show (ExplicitParameter _ qname expr) =
    "(" ++ show qname ++ " : " ++ show expr ++ ")"
  show (ImplicitParameter _ qname expr) =
    "{" ++ show qname ++ " : " ++ show expr ++ "}"

instance Show a => Show (AConstructorDecl a) where
  show (ConstructorDecl _ qname expr) =
    show qname ++ " : " ++ show expr

instance Show a => Show (AExpr a) where
  show (EWildcard _)  = "_"
  show (EHole _ hname) = show hname
  show (EVar _ qname) = show qname
  show (ELam _ qname expr1 expr2 ) =
       "λ (" ++ show qname ++ " : " ++ show expr1 ++ ") → " ++ show expr2
  show (ELamImplicit _ qname expr1 expr2 ) =
       "λ {" ++ show qname ++ " : " ++ show expr1 ++ "} → " ++ show expr2
  show expr@(EApp _ _ _) = showExprApp expr
  show expr@(EAppImplicit _ _ _) = showExprApp expr
  show (ECase _ expr branches) =
    "case " ++ show expr ++ " of\n" ++
    joinS "\n" (map (indent . show) branches)
  show (EPi _ param expr) =
    "∀ " ++ show param ++ " → " ++ show expr
  show (ESig _ param expr) =
    "∃ " ++ show param ++ " × " ++ show expr
  show (EInaccessible _ expr) =
    "." ++ showExprParen expr

showExprApp :: Show a => AExpr a -> String
showExprApp expr = do
    let (head, args) = splitArgs expr in
      joinS " " (map showOptionalExprParen (OptPlain head : args))
  where
    splitArgs :: AExpr a -> (AExpr a, [AOptionalExpr a])
    splitArgs (EApp _ fun arg) =
      let (head, args) = splitArgs fun in
        (head, args ++ [OptPlain arg])
    splitArgs (EAppImplicit _ fun arg) =
      let (head, args) = splitArgs fun in
        (head, args ++ [OptOptional arg])
    splitArgs e = (e, [])

showOptionalExprParen :: Show a => AOptionalExpr a -> String
showOptionalExprParen (OptOptional e) = "{" ++ show e ++ "}"
showOptionalExprParen (OptPlain e)    = showExprParen e

showExprParen :: Show a => AExpr a -> String
showExprParen e@(EWildcard _)       = show e
showExprParen e@(EHole _ _)         = show e
showExprParen e@(EVar _ _)          = show e
showExprParen e@(EInaccessible _ _) = show e
showExprParen e                     = "(" ++ show e ++ ")"

----

instance Show a => Show (ACaseBranch a) where
  show (CaseBranch _ patterns body) =
    joinS " " (map showPatternParen patterns) ++ " → " ++ show body

instance Show a => Show (APattern a) where
  show (PWildcard _)            = "_"
  show (PHole _ hname)          = show hname
  show (PVar _ qname)           = show qname
  show pat@(PApp _ _ _)         = showPatternApp pat
  show pat@(PAppImplicit _ _ _) = showPatternApp pat
  show (PInaccessible _ pat)    = "." ++ showPatternParen pat

showPatternApp :: Show a => APattern a -> String
showPatternApp pat = do
    let (head, args) = splitArgs pat in
      joinS " " ([show head] ++ map showOptionalPatternParen args)
  where
    splitArgs :: APattern a -> (APattern a, [Either (APattern a) (APattern a)])
    splitArgs (PApp _ fun arg) =
      let (head, args) = splitArgs fun in
        (head, args ++ [Right arg])
    splitArgs (PAppImplicit _ fun arg) =
      let (head, args) = splitArgs fun in
        (head, args ++ [Left arg])
    splitArgs e = (e, [])

showOptionalPatternParen :: Show a => Either (APattern a) (APattern a) -> String
showOptionalPatternParen (Left p)  = "{" ++ show p ++ "}"
showOptionalPatternParen (Right p) = showPatternParen p

showPatternParen :: Show a => APattern a -> String
showPatternParen p@(PWildcard _)       = show p
showPatternParen p@(PHole _ _)         = show p
showPatternParen p@(PVar _ _)          = show p
showPatternParen p@(PInaccessible _ _) = show p
showPatternParen p                     = "(" ++ show p ++ ")"

----

exprToPattern :: AExpr a -> Maybe (APattern a)
exprToPattern (EWildcard ann) = return $ PWildcard ann
exprToPattern (EHole ann hname) = return $ PHole ann hname
exprToPattern (EVar ann qname) = return $ PVar ann qname
exprToPattern (EApp ann e1 e2) = do
  p1 <- exprToPattern e1
  p2 <- exprToPattern e2
  return $ PApp ann p1 p2
exprToPattern (EAppImplicit ann e1 e2) = do
  p1 <- exprToPattern e1
  p2 <- exprToPattern e2
  return $ PAppImplicit ann p1 p2
exprToPattern (EInaccessible ann e) = do
  p <- exprToPattern e
  return $ PInaccessible ann p
exprToPattern _ = Nothing

patternToExpr :: APattern a -> AExpr a
patternToExpr (PWildcard ann) = EWildcard ann
patternToExpr (PHole ann hname) = EHole ann hname
patternToExpr (PVar ann qname) = EVar ann qname
patternToExpr (PApp ann e1 e2) =
  EApp ann (patternToExpr e1) (patternToExpr e2)
patternToExpr (PAppImplicit ann e1 e2) =
  EAppImplicit ann (patternToExpr e1) (patternToExpr e2)
patternToExpr (PInaccessible ann p) =
  EInaccessible ann (patternToExpr p)

