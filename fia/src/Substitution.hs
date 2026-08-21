
module Substitution(
         FormSubst, NatSubst, NatRenaming,
         bindPropParamsToArgs, bindFunParamsToArgs,
         substituteForm, substituteNat,
         natRenamingFreeNatVars, freshNatVarId
       ) where

import qualified Data.Set as S
import qualified Data.Map as M

import Syntax(
         HoleId(..), HypId(..), NatVarId(..), FunId(..), PropId(..), FormVarId(..),
         NatVarKind(..),
         Program(..), Declaration(..), Nat(..), Form(..), Proof(..),
         EqChain(..), PropParam(..), PropArg(..),
         formFreeNatVars, natFreeNatVars
       )

type FormSubst = M.Map FormVarId Form

type NatSubst  = M.Map NatVarId Nat

type NatRenaming  = M.Map NatVarId NatVarId

formSubstFreeNatVars :: FormSubst -> S.Set NatVarId
formSubstFreeNatVars fsub =
  S.unions (map (formFreeNatVars . snd) (M.toList fsub))

natSubstDomain :: NatSubst -> S.Set NatVarId
natSubstDomain nsub = S.unions (map (S.singleton . fst) (M.toList nsub))

natSubstFreeNatVars :: NatSubst -> S.Set NatVarId
natSubstFreeNatVars nsub = S.unions (map (natFreeNatVars . snd) (M.toList nsub))

natRenamingFreeNatVars :: NatRenaming -> S.Set NatVarId
natRenamingFreeNatVars nren =
  S.fromList (concatMap (\ (x, y) -> [x, y]) (M.toList nren))

freshNatVarId :: S.Set NatVarId -> NatVarId -> NatVarId
freshNatVarId forbidden natVarId =
  let suffixes = "" : map show [1..]
      candidates = [
        natVarId' 
        | suffix <- suffixes,
          natVarId' <- [NatVarId (natVarIdName natVarId ++ suffix)],
          not (S.member natVarId' forbidden)
        ]
   in head candidates

bindPropParamsToArgs :: [PropParam] -> [PropArg] -> (FormSubst, NatSubst)
bindPropParamsToArgs [] [] = (M.empty, M.empty)
bindPropParamsToArgs (PropParamNat natId : params) (PropArgNat nat : args) =
  let (formSubst, natSubst) = bindPropParamsToArgs params args
   in (formSubst, M.insert natId nat natSubst)
bindPropParamsToArgs (PropParamForm formId : params) (PropArgForm form : args) =
  let (formSubst, natSubst) = bindPropParamsToArgs params args
   in (M.insert formId form formSubst, natSubst)
bindPropParamsToArgs _ _ =
  error "shape of parameters does not match shape of arguments"

bindFunParamsToArgs :: [NatVarId] -> [Nat] -> NatSubst
bindFunParamsToArgs params args = M.fromList (zip params args)

substituteForm :: FormSubst -> NatSubst -> Form -> Form
substituteForm fsub _    (FormVar x)            =
  case M.lookup x fsub of
    Nothing   -> FormVar x
    Just form -> form
substituteForm fsub nsub (FormEq nat1 nat2)     =
  FormEq (substituteNat nsub nat1)
         (substituteNat nsub nat2)
substituteForm fsub nsub (FormOr form1 form2)  =
  FormOr (substituteForm fsub nsub form1)
         (substituteForm fsub nsub form2)
substituteForm fsub nsub (FormAnd form1 form2)  =
  FormAnd (substituteForm fsub nsub form1)
          (substituteForm fsub nsub form2)
substituteForm fsub nsub (FormImp form1 form2)  =
  FormImp (substituteForm fsub nsub form1)
          (substituteForm fsub nsub form2)
substituteForm fsub nsub (FormForall kind n form) =
  substituteFormBinder fsub nsub (FormForall kind) n form
substituteForm fsub nsub (FormProp p args)      =
  FormProp p (map (substitutePropArg fsub nsub) args)
substituteForm fsub nsub (FormExists kind n form) =
  substituteFormBinder fsub nsub (FormExists kind) n form

substituteFormBinder :: FormSubst -> NatSubst
                     -> (NatVarId -> Form -> Form)
                     -> NatVarId
                     -> Form
                     -> Form
substituteFormBinder fsub nsub formForallConstructor n form =
  let forbidden = formSubstFreeNatVars fsub
                  `S.union` natSubstDomain nsub
                  `S.union` natSubstFreeNatVars nsub
      n' = freshNatVarId forbidden n
      nsub' = M.insert n (NatVar n') nsub
    in formForallConstructor n' (substituteForm fsub nsub' form)

substitutePropArg :: FormSubst -> NatSubst -> PropArg -> PropArg
substitutePropArg fsub nsub (PropArgNat nat)   =
  PropArgNat (substituteNat nsub nat)
substitutePropArg fsub nsub (PropArgForm form) =
  PropArgForm (substituteForm fsub nsub form)

substituteNat :: NatSubst -> Nat -> Nat
substituteNat nsub (NatHole holeId) = NatHole holeId
substituteNat nsub (NatVar n) =
  case M.lookup n nsub of
    Nothing  -> NatVar n
    Just nat -> nat
substituteNat nsub NatZero = NatZero
substituteNat nsub (NatSucc nat) = NatSucc (substituteNat nsub nat)
substituteNat nsub (NatAdd nat1 nat2) = NatAdd (substituteNat nsub nat1)
                                               (substituteNat nsub nat2)
substituteNat nsub (NatMul nat1 nat2) = NatMul (substituteNat nsub nat1)
                                               (substituteNat nsub nat2)
substituteNat nsub (NatFun f args) = NatFun f (map (substituteNat nsub) args)

