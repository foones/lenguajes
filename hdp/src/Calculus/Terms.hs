module Calculus.Terms(Decl(..), Kind(..), Type(..), Expr(..)) where

import Position(Position(..))
import Syntax.Name(QName(..), unqualifiedName)

data Decl = DType Position QName Kind [QName] Type
          | DDef Position QName Type Expr

instance Show Decl where
  show (DType _ name kind params typ) =
    unqualifiedName name ++ " : " ++ show kind ++ "\n" ++
    unqualifiedName name ++
    concat (map ((" " ++). unqualifiedName) params) ++
    " = " ++ show typ ++ "\n"
  show (DDef _ name typ term) =
    unqualifiedName name ++ " : " ++ show typ ++ "\n" ++
    unqualifiedName name ++ " = " ++ show term ++ "\n"

data Kind = KType
          | KArrow Kind Kind
  deriving Eq

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

kindLevel :: Kind -> Integer
kindLevel KType        = 60
kindLevel (KArrow _ _) = 50

instance Show Kind where
  show kind = rec kind
    where
      rec KType            = "Type"
      rec k@(KArrow k1 k2) = prec (kindLevel k) k1
                             ++ " → " ++ prec' (kindLevel k) k2
      prec  lev k = (if kindLevel k <= lev then parenthesize else id) (show k)
      prec' lev k = (if kindLevel k < lev  then parenthesize else id) (show k)

data Type = TBase QName
          | TLinImp Type Type
          | TTensor Type Type
          | TWith Type Type
          | TLinSum Type Type
          | TBang Type
          | TForall QName Type
          | TApp Type Type
          | TLam QName Type    -- only used internally

isTBase :: Type -> Bool
isTBase (TBase _) = True
isTBase _         = False

typeLevel :: Type -> Integer
typeLevel (TBase _)     = 100
typeLevel (TLinImp _ _) = 50
typeLevel (TTensor _ _) = 60
typeLevel (TWith _ _)   = 60
typeLevel (TLinSum _ _) = 60
typeLevel (TBang _)     = 70
typeLevel (TForall _ _) = 30
typeLevel (TApp _ _)    = 80
typeLevel (TLam _ _)    = 90

instance Show Type where
  show t = rec t
    where
      rec   (TBase x)       = unqualifiedName x
      rec t@(TLinImp t1 t2) = prec (typeLevel t) t1
                              ++ " → " ++ prec' (typeLevel t) t2
      rec t@(TTensor t1 t2) = prec' (typeLevel t) t1
                              ++ " ⊗ " ++ prec (typeLevel t) t2
      rec t@(TWith t1 t2)   = prec' (typeLevel t) t1
                              ++ " & " ++ prec (typeLevel t) t2
      rec t@(TLinSum t1 t2) = prec' (typeLevel t) t1
                              ++ " ⊕ " ++ prec (typeLevel t) t2
      rec t@(TBang t')      = "!" ++ prec (typeLevel t) t'
      rec t@(TForall _ _)   = let (xs, t') = splitForalls t in
                                "∀ " ++ unwords (map unqualifiedName xs)
                                ++ " : " ++ prec (typeLevel t) t'
      rec t@(TApp t1 t2)    = prec' (typeLevel t) t1
                              ++ " " ++ prec (typeLevel t) t2
      rec t@(TLam _ _)      = let (xs, t') = splitLams t in
                                "λ " ++ unwords (map unqualifiedName xs)
                                ++ " → " ++ prec (typeLevel t) t'
      prec  lev t = (if typeLevel t <= lev then parenthesize else id) (show t)
      prec' lev t = (if typeLevel t < lev  then parenthesize else id) (show t)

      splitForalls :: Type -> ([QName], Type)
      splitForalls (TForall x t) = let (xs, t') = splitForalls t in
                                       (x : xs, t')
      splitForalls t             = ([], t)

      splitLams :: Type -> ([QName], Type)
      splitLams (TLam x t) = let (xs, t') = splitLams t in
                                 (x : xs, t')
      splitLams t          = ([], t)

data Expr = EVar QName
          | EHole String
          | ELam QName Expr
          | EApp Expr Expr
          | ELamT QName Expr
          | EAppT Expr Type
          | ETensorI Expr Expr
          | ETensorE Expr QName QName Expr
          | EWithI Expr Expr
          | EWithE1 Expr QName Expr
          | EWithE2 Expr QName Expr
          | ELinSumI1 Expr
          | ELinSumI2 Expr
          | ELinSumE Expr QName Expr QName Expr
          | EBangI Expr
          | EBangE Expr QName Expr

exprLevel :: Expr -> Integer
exprLevel (EVar _)             = 100
exprLevel (EHole _)            = 100
exprLevel (ELam _ _)           = 40
exprLevel (EApp _ _)           = 70
exprLevel (ELamT _ _)          = 40
exprLevel (EAppT _ _)          = 70
exprLevel (ETensorI _ _)       = 50
exprLevel (ETensorE _ _ _ _)   = 50
exprLevel (EWithI _ _)         = 50
exprLevel (EWithE1 _ _ _)      = 50
exprLevel (EWithE2 _ _ _)      = 50
exprLevel (ELinSumI1 _)        = 50
exprLevel (ELinSumI2 _)        = 50
exprLevel (ELinSumE _ _ _ _ _) = 50
exprLevel (EBangI _)           = 50
exprLevel (EBangE _ _ _)       = 50

instance Show Expr where
  show e = rec e
    where
      rec e@(EVar x)                  = unqualifiedName x
      rec e@(EHole s)                 = show s
      rec e@(ELam _ _)                =
        let (xs, e') = splitLams e in
          "λ " ++ unwords (map unqualifiedName xs)
          ++ " → " ++ show e'
      rec e@(EApp e1 e2)              =
        prec' (exprLevel e) e1 ++ " " ++ prec (exprLevel e) e2
      rec e@(ELamT x e')              =
        let (xs, e') = splitLams e in
          "λ " ++ unwords (map unqualifiedName xs)
          ++ " → " ++ show e'
      rec e@(EAppT e' t)              =
        prec' (exprLevel e) e' ++ " "
        ++ (if isTBase t then id else parenthesize) (show t)
      rec e@(ETensorI e1 e2)          =
        prec' (exprLevel e) e1 ++ " ⊗ " ++ prec (exprLevel e) e2
      rec e@(ETensorE e1 x y e2)      =
        "δ⊗ " ++ precg e1 ++ " "
              ++ "(" ++ unqualifiedName x ++ " " ++ unqualifiedName y
              ++ " : " ++ rec e2 ++ ")"
      rec e@(EWithI e1 e2)            =
        prec' (exprLevel e) e1 ++ ", " ++ prec (exprLevel e) e2
      rec e@(EWithE1 e1 x e2)         =
        "δ&₁ " ++ precg e1 ++ " "
              ++ "(" ++ unqualifiedName x
              ++ " : " ++ rec e2 ++ ")"
      rec e@(EWithE2 e1 x e2)         =
        "δ&₂ " ++ precg e1 ++ " "
              ++ "(" ++ unqualifiedName x
              ++ " : " ++ rec e2 ++ ")"
      rec e@(ELinSumI1 e')            = "inl " ++ prec (exprLevel e) e'
      rec e@(ELinSumI2 e')            = "inr " ++ prec (exprLevel e) e'
      rec e@(ELinSumE e1 x1 e2 x2 e3) =
        "δ⊕ " ++ precg e1 ++ " "
              ++ "(" ++ unqualifiedName x1 ++ " : " ++ rec e2 ++ ")"
              ++ " "
              ++ "(" ++ unqualifiedName x2 ++ " : " ++ rec e3 ++ ")"
      rec e@(EBangI e')               = "!" ++ prec (exprLevel e) e'
      rec e@(EBangE e1 x e2)          =
        "δ! " ++ precg e1 ++ " "
              ++ "(" ++ unqualifiedName x ++ " : " ++ rec e2 ++ ")"

      prec  lev e = (if exprLevel e <= lev then parenthesize else id) (show e)
      prec' lev e = (if exprLevel e < lev  then parenthesize else id) (show e)
      precg e     = (if exprLevel e < 100  then parenthesize else id) (show e)

      splitLams :: Expr -> ([QName], Expr)
      splitLams (ELam x t)  = let (xs, t') = splitLams t in
                                  (x : xs, t')
      splitLams (ELamT x t) = let (xs, t') = splitLams t in
                                  (x : xs, t')
      splitLams t           = ([], t)

