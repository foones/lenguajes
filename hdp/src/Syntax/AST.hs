
module Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr,
         unEVar, PlaceholderId,
         eraseAnnotations, declarationAnnotation, exprAnnotation,
         exprIsVariable, exprHeadVariable, exprHeadArguments,
         exprFunctionType,
         exprIsFunctionType, exprFunctionTypeCodomain, exprEqual,
         exprFreeVariables, splitDatatypeArgsOrFail,
         unfoldPlaceholders
       ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe(fromJust)

import Position(Position)
import Syntax.Name(QName, primitiveArrow)

data AnnProgram a = Program {
                      programDeclarations :: [AnnDeclaration a]
                    }
  deriving Eq

type Program = AnnProgram Position

-- Annotated declaration
data AnnDeclaration a = 
    DataDeclaration {
      dataAnnotation   :: a,
      dataType         :: AnnExpr a,
      dataConstructors :: [AnnSignature a]
    }
  | TypeDeclaration {
      typeAnnotation :: a,
      typeType       :: AnnExpr a,
      typeValue      :: AnnExpr a
    }
  | TypeSignature {
      typeSignature :: AnnSignature a
    }
  | ValueDeclaration {
      declEquation :: AnnEquation a
    }
  | ClassDeclaration {
      classAnnotation :: a,
      className       :: QName,
      classTypeName   :: QName,
      classMethods    :: [AnnSignature a]
    }
  | InstanceDeclaration {
      instanceAnnotation  :: a,
      instanceClassName   :: QName,
      instanceType        :: AnnExpr a,
      instanceConstraints :: [AnnConstraint a],
      instanceMethods     :: [AnnEquation a]
    }
  | MutualDeclaration {
      mutualAnnotation    :: a,
      mutualDeclarations  :: [AnnDeclaration a]
    }
  | EvalDeclaration {
      evalAnnotation :: a,
      evalExpression :: AnnExpr a
    }
  deriving Eq

declarationAnnotation :: AnnDeclaration a -> a
declarationAnnotation (DataDeclaration a _ _)         = a
declarationAnnotation (TypeDeclaration a _ _)         = a
declarationAnnotation (TypeSignature s)               = signatureAnnotation s
declarationAnnotation (ValueDeclaration e)            = equationAnnotation e
declarationAnnotation (ClassDeclaration a _ _ _)      = a
declarationAnnotation (InstanceDeclaration a _ _ _ _) = a
declarationAnnotation (MutualDeclaration a _)         = a
declarationAnnotation (EvalDeclaration a _)           = a

data AnnSignature a = Signature {
                        signatureAnnotation  :: a,
                        signatureName        :: QName,
                        signatureType        :: AnnExpr a,
                        signatureConstraints :: [AnnConstraint a]
                      } deriving Eq

data AnnEquation a = Equation {
                       equationAnnotation :: a,
                       equationLHS        :: AnnExpr a,
                       equationRHS        :: AnnExpr a
                     } deriving Eq

data AnnConstraint a = Constraint {
                         constraintAnnotation :: a,
                         constraintClassName  :: QName,
                         constraintTypeName   :: QName
                       } deriving Eq

data AnnCaseBranch a = CaseBranch {
                         caseBranchAnnotation :: a,
                         caseBranchPattern    :: AnnExpr a,
                         caseBranchResult     :: AnnExpr a
                       } deriving Eq

-- Annotated expression
type PlaceholderId = Integer
data AnnExpr a =
    EVar a QName                           -- variable
  | EUnboundVar a QName                    -- force unbound variable
  | EInt a Integer                         -- integer constant
  | EChar a Char                           -- character constant
  | EString a String                       -- string constant
  | EApp a (AnnExpr a) (AnnExpr a)         -- application
  | ELambda a (AnnExpr a) (AnnExpr a)      -- lambda
  | ELet a [AnnDeclaration a] (AnnExpr a)  -- let
  | ECase a (AnnExpr a) [AnnCaseBranch a]  -- case
  | EFresh a QName (AnnExpr a)             -- fresh
  | EPlaceholder a PlaceholderId           -- placeholder for instances
  deriving Eq

exprAnnotation :: AnnExpr a -> a
exprAnnotation (EVar a _)         = a
exprAnnotation (EUnboundVar a _)  = a
exprAnnotation (EInt a _)         = a
exprAnnotation (EChar a _)        = a
exprAnnotation (EString a _)      = a
exprAnnotation (EApp a _ _)       = a
exprAnnotation (ELambda a _ _)    = a
exprAnnotation (ELet a _ _)       = a
exprAnnotation (ECase a _ _)      = a
exprAnnotation (EFresh a _ _)     = a
exprAnnotation (EPlaceholder a _) = a

exprFunctionType :: AnnExpr a -> AnnExpr a -> AnnExpr a
exprFunctionType dom cod =
    EApp ann (EApp ann (EVar ann primitiveArrow) dom) cod
  where
    ann = exprAnnotation dom

exprIsFunctionType :: AnnExpr a -> Bool
exprIsFunctionType (EApp _ (EApp _ (EVar _ op) _) _) = op == primitiveArrow
exprIsFunctionType _                                 = False

exprFunctionTypeCodomain :: AnnExpr a -> AnnExpr a
exprFunctionTypeCodomain (EApp _ (EApp _ (EVar _ _) _) x) = x
exprFunctionTypeCodomain _ = error "(Not a function type)"

-- Check whether expressions are equal ignoring annotations
exprEqual :: AnnExpr a -> AnnExpr a -> Bool
exprEqual e1 e2 = eraseAnnotations e1 == eraseAnnotations e2

--

type Declaration = AnnDeclaration Position
type Constraint  = AnnConstraint Position
type Signature   = AnnSignature Position
type Equation    = AnnEquation Position
type CaseBranch  = AnnCaseBranch Position
type Expr        = AnnExpr Position

--

class EraseAnnotations f where
  eraseAnnotations :: f a -> f ()

instance EraseAnnotations AnnProgram where
  eraseAnnotations (Program x) = Program (map eraseAnnotations x)

instance EraseAnnotations AnnDeclaration where
  eraseAnnotations (DataDeclaration _ x y) =
    DataDeclaration () (eraseAnnotations x) (map eraseAnnotations y)
  eraseAnnotations (TypeDeclaration _ x y) =
    TypeDeclaration () (eraseAnnotations x) (eraseAnnotations y)
  eraseAnnotations (TypeSignature x) =
    TypeSignature (eraseAnnotations x)
  eraseAnnotations (ValueDeclaration x) =
    ValueDeclaration (eraseAnnotations x)
  eraseAnnotations (ClassDeclaration _ x y z) =
    ClassDeclaration () x y (map eraseAnnotations z)
  eraseAnnotations (InstanceDeclaration _ x y z w) =
    InstanceDeclaration () x (eraseAnnotations y)
                             (map eraseAnnotations z)
                             (map eraseAnnotations w)
  eraseAnnotations (MutualDeclaration _ x) =
    MutualDeclaration () (map eraseAnnotations x)
  eraseAnnotations (EvalDeclaration _ e) =
    EvalDeclaration () (eraseAnnotations e)

instance EraseAnnotations AnnSignature where
  eraseAnnotations (Signature _ x y z) =
    Signature () x (eraseAnnotations y) (map eraseAnnotations z)

instance EraseAnnotations AnnEquation where
  eraseAnnotations (Equation _ x y) =
    Equation () (eraseAnnotations x) (eraseAnnotations y)

instance EraseAnnotations AnnConstraint where
  eraseAnnotations (Constraint _ x y) = Constraint () x y

instance EraseAnnotations AnnCaseBranch where
  eraseAnnotations (CaseBranch _ p r) =
    CaseBranch () (eraseAnnotations p) (eraseAnnotations r)

instance EraseAnnotations AnnExpr where
  eraseAnnotations (EVar _ q)          = EVar () q
  eraseAnnotations (EUnboundVar _ q)   = EUnboundVar () q
  eraseAnnotations (EInt _ n)          = EInt () n
  eraseAnnotations (EChar _ c)         = EChar () c
  eraseAnnotations (EString _ s)       = EString () s
  eraseAnnotations (EApp _ e1 e2)      = EApp () (eraseAnnotations e1)
                                                 (eraseAnnotations e2)
  eraseAnnotations (ELambda _ param e) = ELambda ()
                                                 (eraseAnnotations param)
                                                 (eraseAnnotations e)
  eraseAnnotations (ELet _ ds e)       = ELet () (map eraseAnnotations ds)
                                                 (eraseAnnotations e)
  eraseAnnotations (ECase _ e bs)      = ECase () (eraseAnnotations e)
                                                  (map eraseAnnotations bs)
  eraseAnnotations (EFresh _ name e)   = EFresh () name (eraseAnnotations e)
  eraseAnnotations (EPlaceholder _ id) = EPlaceholder () id

--

exprIsVariable :: AnnExpr a -> Bool
exprIsVariable (EVar _ _) = True
exprIsVariable _          = False

exprHeadVariable :: AnnExpr a -> Maybe QName
exprHeadVariable (EVar _ q)           = return q
exprHeadVariable (EUnboundVar _ q)    = return q
exprHeadVariable (EApp _ e1 _)        = exprHeadVariable e1
exprHeadVariable _                    = Nothing

exprHeadArguments :: AnnExpr a -> Maybe [AnnExpr a]
exprHeadArguments (EVar _ _)          = return []
exprHeadArguments (EUnboundVar _ _)   = return []
exprHeadArguments (EApp _ e1 e2)      = do
    args <- exprHeadArguments e1
    return (args ++ [e2])
exprHeadArguments _                   = Nothing

exprFreeVariables :: S.Set QName -> AnnExpr a -> S.Set QName
exprFreeVariables bound (EVar _ x)        = if x `S.member` bound
                                             then S.empty
                                             else S.fromList [x]
exprFreeVariables bound (EUnboundVar _ x) = S.fromList [x]
exprFreeVariables bound (EInt _ _)        = S.empty
exprFreeVariables bound (EChar _ _)       = S.empty
exprFreeVariables bound (EString _ _)     = S.empty
exprFreeVariables bound (EApp _ e1 e2)    = exprFreeVariables bound e1
                                            `S.union`
                                            exprFreeVariables bound e2
exprFreeVariables bound (ELambda _ e1 e2) =
  exprFreeVariables
    (bound `S.union` exprFreeVariables bound e1)
    e2
exprFreeVariables bound (ELet _ declarations body) =
  let functionNames = S.unions (map declBoundVariables declarations)
   in S.unions (map (declFreeVariables bound functionNames) declarations)
      `S.union`
      exprFreeVariables (S.union bound functionNames) body
  where
    declFreeVariables :: S.Set QName -> S.Set QName -> AnnDeclaration a
                      -> S.Set QName
    declFreeVariables bound functionNames
                      (ValueDeclaration (Equation _ lhs rhs)) =
      let args       = fromJust (exprHeadArguments lhs)
          freeInArgs = S.unions (map (exprFreeVariables bound) args)
       in exprFreeVariables (S.unions [bound, functionNames, freeInArgs]) rhs
    declFreeVariables _ _ _ = S.empty
    --
    declBoundVariables :: AnnDeclaration a -> S.Set QName
    declBoundVariables (ValueDeclaration (Equation _ lhs _)) =
      let f = fromJust (exprHeadVariable lhs)
       in S.fromList [f]
    declBoundVariables _ = S.empty
exprFreeVariables bound (ECase _ guard branches) =
  let guardFV  = exprFreeVariables bound guard
      branchFV = S.unions $ map (branchFreeVariables bound) branches
  in S.union guardFV branchFV
exprFreeVariables bound (EFresh _ x e)     = exprFreeVariables
                                               (S.insert x bound)
                                               e
exprFreeVariables bound (EPlaceholder _ _) =
  error "(Free variables of a placeholder should not be requested)"

branchFreeVariables :: S.Set QName -> AnnCaseBranch a -> S.Set QName
branchFreeVariables bound (CaseBranch _ pattern body) =
  let patternFV = exprFreeVariables bound pattern
  in exprFreeVariables (S.union bound patternFV) body

---- Show

joinS :: String -> [String] -> String
joinS _   []       = ""
joinS _   [l]      = l
joinS sep (l : ls) = l ++ sep ++ joinS sep ls

joinLines :: [String] -> String
joinLines = joinS "\n"

indent :: String -> String
indent s = "  " ++ s

instance Show (AnnProgram a) where
  show (Program decls) = joinS "\n\n" (map show decls)

instance Show (AnnDeclaration a) where
  show (DataDeclaration _ typ constructorDeclarations) =
    joinLines (
      ["data " ++ show typ ++ " where"] ++
      map (indent . show) constructorDeclarations
    )
  show (TypeDeclaration _ typ val) =
    "type " ++ show typ ++ " = " ++ show val
  show (TypeSignature sig) = show sig
  show (ValueDeclaration equation) = show equation
  show (ClassDeclaration _ name typeName signatures) =
    joinLines (
      ["class " ++ show name ++ " " ++ show typeName ++ " where"] ++
      map (indent . show) signatures
    )
  show (InstanceDeclaration _ name typ constraints equations) =
    joinLines (
      ["instance " ++ show name ++ " " ++ show typ ++
                      showOptionalConstraints constraints ++ " where"] ++
      map (indent . show) equations
    )
  show (MutualDeclaration _ mutualDeclarations) =
    joinLines (
      ["mutual"] ++
      map (indent . show) mutualDeclarations
    )
  show (EvalDeclaration _ expr) =
    "eval " ++ show expr

instance Show (AnnSignature a) where
  show (Signature _ name typ constraints) =
      show name ++ " : " ++ show typ ++ showOptionalConstraints constraints

instance Show (AnnEquation a) where
  show (Equation _ lhs rhs) = show lhs ++ " = " ++ show rhs

instance Show (AnnConstraint a) where
  show (Constraint _ className typeName) =
    show className ++ " " ++ show typeName

instance Show (AnnCaseBranch a) where
  show (CaseBranch _ p r) =
    show p ++ " -> " ++ show r

showOptionalConstraints :: [AnnConstraint a] -> String
showOptionalConstraints [] = ""
showOptionalConstraints cs = 
  indent ("{"
       ++ joinS "; " (map show cs)
       ++ "}")

instance Show (AnnExpr a) where
  show (EVar _ qname)         = show qname
  show (EUnboundVar _ qname)  = "." ++ show qname
  show (EInt _ n)             = show n
  show (EChar _ c)            = show c
  show (EString _ s)          = show s
  show (EApp _ f x)           = "(" ++ show f ++ " " ++ show x ++ ")"
  show (ELambda _ param body) =
    "\\ " ++ show param ++ " -> " ++ show body
  show (ELet _ ds e)          =
    "(let {" ++ joinS "; " (map show ds) ++ "} in " ++ show e ++ ")"
  show (ECase _ e branches)    =
    "(case " ++ show e ++ " of {" ++ (joinS "; " (map show branches)) ++ "})"
  show (EFresh _ name e)      =
    "(fresh " ++ show name ++ " in " ++ show e ++ ")"
  show (EPlaceholder _ id)      =
    "{PLACEHOLDER_" ++ show id ++ "}"

-- Split the left-hand side in the definition of a datatype,
-- such as "Map a b" into the head "Map" and the arguments ["a", "b"]
-- checking that it is well-formed.
--
-- All the arguments should be variables.
splitDatatypeArgsOrFail :: Expr -> (QName, [QName])
splitDatatypeArgsOrFail (EApp _ f (EVar _ x)) =
  let (head, args) = splitDatatypeArgsOrFail f
   in (head, args ++ [x])
splitDatatypeArgsOrFail (EVar _ x) = (x, [])
splitDatatypeArgsOrFail _ = error "(Malformed datatype)"

---- Unfold all placeholders inside an expression 

type PlaceholderHeap a = M.Map PlaceholderId (AnnExpr a)

class UnfoldPlaceholders f where
  unfoldPlaceholders :: PlaceholderHeap a -> f a -> Either (AnnExpr a) (f a)

instance UnfoldPlaceholders AnnProgram where
  unfoldPlaceholders h (Program decls) =
    Program <$> mapM (unfoldPlaceholders h) decls

instance UnfoldPlaceholders AnnDeclaration where
  unfoldPlaceholders h d@(DataDeclaration _ _ _)    = return d
  unfoldPlaceholders h d@(TypeDeclaration _ _ _)    = return d
  unfoldPlaceholders h d@(TypeSignature _)          = return d
  unfoldPlaceholders h (ValueDeclaration equation)  =
    ValueDeclaration <$> unfoldPlaceholders h equation
  unfoldPlaceholders h d@(ClassDeclaration _ _ _ _) = return d
  unfoldPlaceholders h d@(InstanceDeclaration a name typ cs eqs) = do
    InstanceDeclaration a name typ cs <$>
      mapM (unfoldPlaceholders h) eqs
  unfoldPlaceholders h d@(MutualDeclaration a ds) = do
    MutualDeclaration a <$> mapM (unfoldPlaceholders h) ds
  unfoldPlaceholders h (EvalDeclaration a expr) = do
    EvalDeclaration a <$> unfoldPlaceholders h expr

instance UnfoldPlaceholders AnnEquation where
  unfoldPlaceholders h (Equation a lhs rhs) =
    Equation a <$> unfoldPlaceholders h lhs
               <*> unfoldPlaceholders h rhs

instance UnfoldPlaceholders AnnExpr where
  unfoldPlaceholders h e@(EVar _ _)        = return e
  unfoldPlaceholders h e@(EUnboundVar _ _) = return e
  unfoldPlaceholders h e@(EInt _ _)        = return e
  unfoldPlaceholders h e@(EChar _ _)       = return e
  unfoldPlaceholders h e@(EString _ _)     = return e
  unfoldPlaceholders h (EApp a e1 e2)      =
    EApp a <$> unfoldPlaceholders h e1
           <*> unfoldPlaceholders h e2
  unfoldPlaceholders h (ELambda a e1 e2)   =
    ELambda a <$> unfoldPlaceholders h e1
              <*> unfoldPlaceholders h e2
  unfoldPlaceholders h (ELet a decls e)    =
    ELet a <$> mapM (unfoldPlaceholders h) decls
           <*> unfoldPlaceholders h e
  unfoldPlaceholders h (ECase a e bs)      =
    ECase a <$> unfoldPlaceholders h e
            <*> mapM (unfoldPlaceholders h) bs
  unfoldPlaceholders h (EFresh a x e)      =
    EFresh a x <$> unfoldPlaceholders h e
  unfoldPlaceholders h (EPlaceholder a p)  =
    if M.member p h
     then unfoldPlaceholders h (M.findWithDefault undefined p h)
     else Left (EPlaceholder a p)

instance UnfoldPlaceholders AnnCaseBranch where
  unfoldPlaceholders h (CaseBranch a e1 e2) =
    CaseBranch a <$> unfoldPlaceholders h e1
                 <*> unfoldPlaceholders h e2

unEVar :: Expr -> QName
unEVar (EVar _ x)        = x
unEVar (EUnboundVar _ x) = x
unEVar _                 = error "(Not a variable)"

