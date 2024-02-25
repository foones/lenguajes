module ModuleSystem.Context(
         Context,
           emptyContext, contextCurrentModuleName,
           importAllNamesFromModule, importNames,
           declareModuleAlias, resolveName, unresolveName
       ) where

import qualified Data.Set as S
import qualified Data.Map as M

import Error(ErrorMessage)
import Syntax.Name(QName(..), qualify,
                   moduleNameFromQName, unqualifiedName,
                   isWellFormedOperatorName, allNameParts)
import ModuleSystem.Module(Module, moduleExists, nameIsExported)

---- Context ----

---- A context represents a scope in which some modules may or may
---- not be imported

data Context = Context {
                 contextCurrentModuleName :: QName,
                 contextImportedNames     :: M.Map String (S.Set QName),
                 contextImportedModules   :: S.Set QName,
                 contextModuleAliases     :: M.Map String (S.Set QName)
               }

emptyContext :: QName -> Context
emptyContext currentModuleName =
  Context {
    contextCurrentModuleName = currentModuleName,
    contextImportedNames     = M.empty,
    contextImportedModules   = S.fromList [currentModuleName],
    contextModuleAliases     = M.empty
  }

importAllNamesFromModule :: QName -> Module -> Context
                         -> Either ErrorMessage Context
importAllNamesFromModule moduleName m c = do
  if moduleExists moduleName m
   then
     return (c {
       contextImportedModules = S.insert moduleName (contextImportedModules c)
     })
   else
     Left ("Module \"" ++ show moduleName ++ "\" does not exist.")

importSingleName :: QName -> String -> Module -> Context
                  -> Either ErrorMessage Context
importSingleName originalName alias m c =
  if nameIsExported originalName m
   then
     let ns = contextImportedNames c in
       return (c {
         contextImportedNames =
           M.insert alias (S.insert originalName
                                    (M.findWithDefault S.empty alias ns))
                          ns
       })
   else
     Left ("Name \"" ++ show originalName ++ "\" is not exported.")

importNames :: QName -> [(String, String)] -> Module -> Context
            -> Either ErrorMessage Context
importNames moduleName []                  _ c = return c
importNames moduleName ((id, alias) : ids) m c
  | not (isWellFormedOperatorName id) = do
      c' <- importSingleName (qualify moduleName id) alias m c
      importNames moduleName ids m c'
  | isWellFormedOperatorName id && id /= alias =
    Left ("Cannot rename operator: \"" ++ id ++ "\" to \"" ++ alias ++ "\".")
  | otherwise = {- isWellFormedOperatorName id && id == alias -}
      rec (allNameParts id) c
    where
      rec []             c = return c
      rec (part : parts) c = do
        c' <- importSingleName (qualify moduleName part) part m c
        rec parts c'

declareModuleAlias :: QName -> String -> Module -> Context
                   -> Either ErrorMessage Context
declareModuleAlias moduleName alias m c =
  if moduleExists moduleName m
   then
     let ns = contextModuleAliases c in
       return (c {
         contextModuleAliases =
           M.insert alias
                    (S.insert moduleName
                      (M.findWithDefault S.empty alias ns))
                    ns
       })
   else
     Left ("Module \"" ++ show moduleName ++ "\" does not exist.")

resolveName :: Module -> Context -> QName -> Either ErrorMessage QName
resolveName m c (Name id) =
    if null candidates
     then return $ qualify (contextCurrentModuleName c) id
     else
       if null (tail candidates)
        then return $ head candidates
        else Left ("Ambiguous unqualified name: \"" ++ id ++ "\".\n" ++
                   "Candidates:\n" ++
                   unlines (map show candidates))
  where
    candidates =
       S.toList (explicitlyImportedName `S.union` implicitlyImportedNames)
    -- Explicitly imported name 
    explicitlyImportedName =
      M.findWithDefault S.empty id (contextImportedNames c)
    -- Implicitly imported name (by importing all from a module)
    implicitlyImportedNames =
      S.fromList [ qualifiedName |
         submoduleName <- S.toList (contextImportedModules c),
         qualifiedName <- [qualify submoduleName id],
         nameIsExported qualifiedName m
      ]
resolveName m c qname =
  if null candidates
   then Left ("Name: \"" ++ show qname ++ "\" does not exist.")
   else
     if null (tail candidates)
      then return $ head candidates
      else Left ("Ambiguous qualified name: \"" ++ show qname ++ "\".\n" ++
                 "Candidates:\n" ++
                 unlines (map show candidates))
  where
    candidates =
      S.toList (directlyReferencedName `S.union` indirectlyReferencedNames)
    -- Name referenced directly by the name of the module 
    directlyReferencedName =
      if nameIsExported qname m
        then S.fromList [qname]
        else S.empty
    -- Names referenced indirectly by giving an alias to a module
    indirectlyReferencedNames =
      let barename = unqualifiedName qname
          qalias   = moduleNameFromQName qname
       in case qalias of
            -- Alias must be a single identifier
            Name alias ->
              S.fromList [ qualifiedName |
                submoduleName <- S.toList (M.findWithDefault S.empty
                                          alias
                                          (contextModuleAliases c)),
                qualifiedName <- [qualify submoduleName barename],
                nameIsExported qualifiedName m
              ]
            _ -> S.empty

unresolveName :: Module -> Context -> QName -> QName
unresolveName m c qname =
    let barename = Name (unqualifiedName qname) in
      case resolveName m c barename of
        Right qname' | qname == qname' -> barename
        _ -> qname

