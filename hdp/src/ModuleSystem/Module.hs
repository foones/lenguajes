module ModuleSystem.Module(
         Module,
           emptyModule, addSubmodule, declareName,
           exportAllNamesFromModule, exportNames,
           moduleExists, nameIsExported
       ) where

import qualified Data.Set as S
import qualified Data.Map as M

import Error(ErrorMessage)
import Syntax.Name(
         QName(..), qualify, allNameParts
       )

---- A module represents a tree of namespaces

data ModuleExports = ExportAll
                   | ExportSome (S.Set String)

data Module = Module {
                moduleNames           :: S.Set String,
                moduleExportedNames   :: ModuleExports,
                moduleSubmodules      :: M.Map String Module
              }

emptyModule :: Module
emptyModule = Module {
                moduleNames           = S.empty,
                moduleExportedNames   = ExportSome S.empty,
                moduleSubmodules      = M.empty
              }

addSubmodule :: QName -> Module -> Either ErrorMessage Module
addSubmodule =
  liftToQName (\ id m ->
    if M.member id (moduleSubmodules m)
     then Left ("Module \"" ++ id ++ "\" already exists.")
     else return (ensureSubmodule m id)
  )

declareName :: QName -> Module -> Either ErrorMessage Module
declareName =
  liftToQName (\ id m ->
     return (m {
       moduleNames = moduleNames m `S.union`
                     S.fromList (allNameParts id)
     })
  )

exportAllNamesFromModule :: QName -> Module -> Either ErrorMessage Module
exportAllNamesFromModule qname =
  liftToQName (\ _ m -> return (m { moduleExportedNames = ExportAll }))
              (qualify qname "_")

exportSingleName :: QName -> Module -> Either ErrorMessage Module
exportSingleName =
  liftToQName (\ id m ->
    return (m {
      moduleNames = S.insert id (moduleNames m),
      moduleExportedNames =
        let s = (case moduleExportedNames m of
                  ExportAll    -> S.empty
                  ExportSome s -> s)
         in ExportSome (s `S.union` S.fromList (allNameParts id))
    }))

exportNames :: QName -> [String] -> Module -> Either ErrorMessage Module
exportNames moduleName []         m = return m
exportNames moduleName (id : ids) m = do
  m' <- exportSingleName (qualify moduleName id) m
  exportNames moduleName ids m'

--

liftToQName :: (String -> Module -> Either ErrorMessage Module)
            -> QName -> Module -> Either ErrorMessage Module
liftToQName f (Name id) m = f id m
liftToQName f (Qualified id qname) m =
  let m'   = ensureSubmodule m id
      sub  = M.findWithDefault undefined id (moduleSubmodules m')
   in do
        sub' <- liftToQName f qname sub
        return (m' {
                  moduleSubmodules = M.insert id sub' (moduleSubmodules m')
                })

ensureSubmodule :: Module -> String -> Module
ensureSubmodule m id
  | M.member id (moduleSubmodules m) = m
  | otherwise =
      m {
        moduleSubmodules = M.insert id emptyModule (moduleSubmodules m)
      }

moduleExists :: QName -> Module -> Bool
moduleExists (Name id)            m = M.member id (moduleSubmodules m)
moduleExists (Qualified id qname) m =
  M.member id (moduleSubmodules m) &&
  moduleExists qname (M.findWithDefault undefined id (moduleSubmodules m))

getModule :: QName -> Module -> Module
getModule (Name id)            m =
  M.findWithDefault undefined id (moduleSubmodules m)
getModule (Qualified id qname) m =
  getModule qname (M.findWithDefault undefined id (moduleSubmodules m))

nameIsExported :: QName -> Module -> Bool
nameIsExported (Name id) m =
  let e = case moduleExportedNames m of
            ExportAll    -> moduleNames m
            ExportSome s -> s
   in S.member id e
nameIsExported (Qualified id qname) m =
  M.member id (moduleSubmodules m) &&
  nameIsExported qname (M.findWithDefault undefined id (moduleSubmodules m))

