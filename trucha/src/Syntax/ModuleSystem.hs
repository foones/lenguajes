module Syntax.ModuleSystem(
         ModuleSystem, ModuleName,
         initialRootModule, moduleExists, addModule, moduleImport,
         registerLocalName, resolveName,
         Exports(..), Imports(..)
       ) where

import qualified Data.Map as M
import qualified Data.Set as S

import FailState(FailState, evalFS, getFS, putFS, failFS)

import Syntax.Name(
         ModuleName, makeModuleName, moduleNameParts, rootModuleName,
         QName, makeQName, qnameModuleName, qnameBareName, isBareQName,
         BareName, makeBareName, bareNameNamedParts
       )

data ModuleSystem = ModuleSystem {
                      isDefined     :: Bool
                    , submodules    :: M.Map String ModuleSystem
                    , exportedNames :: Exports
                    , localNames    :: S.Set BareName
                    , localImports  :: [(ModuleName, Imports)]
                    }

data Exports = ExportAll
             | ExportNames [BareName]

data Imports = ImportAll
             | ImportNames [BareName]
             | ImportQualified ModuleName

emptyModule :: ModuleSystem
emptyModule =
  ModuleSystem {
    isDefined     = False
  , submodules    = M.empty
  , exportedNames = ExportNames []
  , localNames    = S.empty
  , localImports  = []
  }

initialRootModule :: ModuleSystem
initialRootModule = emptyModule {
                      isDefined  = True
                    }

moduleExists :: ModuleSystem -> ModuleName -> Bool
moduleExists root moduleName = rec root (moduleNameParts moduleName)
  where
    rec ms [] = isDefined ms
    rec ms (prefix : suffix) =
      case M.lookup prefix (submodules ms) of
        Nothing  -> False
        Just ms' -> rec ms' suffix

addModule :: ModuleSystem -> ModuleName -> Exports -> Either String ModuleSystem
addModule root moduleName exports = do
    -- Check whether imported module exists.
    if moduleExists root moduleName
     then Left ("Module " ++ show moduleName ++ " has already been defined.")
     else return ()
    rec root (moduleNameParts moduleName)
  where
    rec ms [] = return $ ms {
                  isDefined = True
                , exportedNames = exportsClosure exports
                }
    rec ms (prefix : suffix) = do
      submodule' <- case M.lookup prefix (submodules ms) of
                      Nothing        -> rec emptyModule suffix
                      Just submodule -> rec submodule suffix
      return $ ms { submodules = M.insert prefix submodule' (submodules ms) }
    exportsClosure :: Exports -> Exports
    exportsClosure ExportAll = ExportAll
    exportsClosure (ExportNames barenames) =
      ExportNames (barenames ++ concatMap bareNameNamedParts barenames)

lookupModule :: ModuleSystem -> ModuleName -> Either String ModuleSystem
lookupModule root moduleName = rec root (moduleNameParts moduleName)
  where
    rec ms [] = return ms
    rec ms (prefix : suffix) = do
      case M.lookup prefix (submodules ms) of
        Nothing -> Left ("Module " ++ show moduleName ++ " does not exist.")
        Just submodule -> rec submodule suffix

modifyModule :: ModuleSystem -> ModuleName -> (ModuleSystem -> ModuleSystem)
             -> ModuleSystem
modifyModule root moduleName f = rec root (moduleNameParts moduleName)
  where
    rec ms [] = f ms
    rec ms (prefix : suffix) =
      let submodule  = M.findWithDefault (error "Module does not exist.")
                                         prefix (submodules ms)
          submodule' = rec submodule suffix
       in ms {
            submodules = M.insert prefix submodule' (submodules ms)
          }

moduleImport :: ModuleSystem -> ModuleName -> ModuleName -> Imports
             -> Either String ModuleSystem
moduleImport root currentModuleName importedModuleName imports = do
  if moduleExists root importedModuleName
   then return ()
   else Left ("Module " ++ show importedModuleName ++ " does not exist.")
  let imports' = importsClosure imports
  return $ modifyModule root currentModuleName
    (\ sub -> sub {
      localImports = localImports sub ++ [(importedModuleName, imports')]
    })
  where
    importsClosure :: Imports -> Imports
    importsClosure ImportAll = ImportAll
    importsClosure (ImportNames barenames) =
      ImportNames (barenames ++ concatMap bareNameNamedParts barenames)
    importsClosure (ImportQualified prefix) = ImportQualified prefix

registerLocalName :: ModuleSystem -> ModuleName -> BareName
                  -> Either String ModuleSystem
registerLocalName root moduleName bareName = do
    let names = bareName : bareNameNamedParts bareName
    return $
      modifyModule root moduleName
        (\ sub -> sub {
                    localNames = S.fromList names `S.union` localNames sub
                  })

parentModuleName :: ModuleName -> Maybe ModuleName
parentModuleName moduleName =
  let parts = moduleNameParts moduleName in
    if null parts
     then Nothing
     else Just (makeModuleName (init parts))

moduleExports :: ModuleSystem -> ModuleName -> BareName -> Bool
moduleExports root moduleName bareName =
  case lookupModule root moduleName of
    Left _ -> False
    Right mod ->
      case exportedNames mod of
        ExportAll -> bareName `elem` localNames mod
        ExportNames names -> bareName `elem` names

type M = FailState (S.Set (ModuleName, QName))

perform :: Either String a -> M a
perform x = case x of
              Left msg -> failFS msg 
              Right a  -> return a

resolveName :: ModuleSystem -> ModuleName -> QName -> Either String QName
resolveName root originalModuleName originalLookedUpName =
    case evalFS S.empty (rec originalModuleName originalLookedUpName) of
      Left msg -> Left msg
      Right x  -> x
  where
    infixr 5 `andThen`
    rec :: ModuleName -> QName -> M (Either String QName)
    rec currentModuleName lookedUpName = do
      visited <- getFS
      let key = (currentModuleName, lookedUpName) in
        if key `S.member` visited
         then return $ Left "Recursive dependency in imports"
         else do putFS (S.insert key visited)
                 rec' currentModuleName lookedUpName

    rec' :: ModuleName -> QName -> M (Either String QName)
    rec' currentModuleName lookedUpName = do
      let lookedUpBareName   = qnameBareName lookedUpName
      let lookedUpModuleName = qnameModuleName lookedUpName
      currentModule <- perform $ lookupModule root currentModuleName
      let tryBareNameDefinedLocally =
            isBareQName lookedUpName
            `andThen`
            (lookedUpBareName `S.member` localNames currentModule)
            `andThen`
            (return . Right $ makeQName currentModuleName lookedUpBareName)
      let tryBareNameInParentModule =
            isBareQName lookedUpName
            `andThen`
            case parentModuleName currentModuleName of
              Nothing -> return $
                           Left ("Name " ++ show lookedUpName
                                 ++ " has not been declared in "
                                 ++ show originalModuleName ++ ".")
              Just parentModuleName -> rec parentModuleName lookedUpName
      let tryImportedBareName remoteModuleName =
            isBareQName lookedUpName
            `andThen`
            moduleExports root remoteModuleName lookedUpBareName
            `andThen`
            rec remoteModuleName (makeQName rootModuleName lookedUpBareName)
      let tryImportedQualifiedName prefix remoteModuleName =
            (lookedUpModuleName == prefix)
            `andThen`
            moduleExports root remoteModuleName lookedUpBareName
            `andThen`
            rec remoteModuleName (makeQName rootModuleName lookedUpBareName)
      let fail = return $
                  Left ("Name " ++ show originalLookedUpName
                        ++ " cannot be resolved in " ++ show originalModuleName ++ ".")
      let importedModulesForBareName =
            [remoteModuleName |
              (remoteModuleName, importPolicy) <- localImports currentModule,
              case importPolicy of
                ImportAll         -> True
                ImportQualified _ -> False
                ImportNames names -> lookedUpBareName `elem` names]
      let importedModulesForQualifiedName =
            [(prefix, remoteModuleName) |
              (remoteModuleName, ImportQualified prefix) <- localImports currentModule]
      let alternatives =
               [tryBareNameDefinedLocally]
            ++ [tryBareNameInParentModule]
            ++ [tryImportedBareName remoteModuleName
               | remoteModuleName <- importedModulesForBareName]
            ++ [tryImportedQualifiedName prefix remoteModuleName
               | (prefix, remoteModuleName) <- importedModulesForQualifiedName]
            ++ [fail]
      alternatives' <- mapM id alternatives
      return $ tryAlternatives alternatives'

    tryAlternatives :: Show b => [Either String b] -> Either String b
    tryAlternatives []              = error "(Impossible)"
    tryAlternatives [e]             = e
    tryAlternatives (Left msg : es) =
      case tryAlternatives es of
        Right res -> return $ res
        Left msg' ->
          if null msg
           then Left $ msg'
           else Left $ msg
    tryAlternatives (Right x : es)  = do
      let otherPossibilities = [y | Right y <- es]
      if null otherPossibilities
       then Right $ x
       else Left $ unlines ([
                "Ambiguous occurrence of operator " ++ show originalLookedUpName
              , "It may refer to:"
              ] ++ map show (x : otherPossibilities))

    andThen :: Bool -> M (Either String b) -> M (Either String b)
    andThen cond e =
      if cond 
       then e
       else return . Left $ ""

