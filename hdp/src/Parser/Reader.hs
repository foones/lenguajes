
module Parser.Reader(readSource) where

-- Read and tokenize a file, including its dependencies.

import System.Directory(doesFileExist)
import qualified Data.Set as S

import Position(unknownPosition)
import Error(Error(..), ErrorType(..), errorAtUnknown)
import Syntax.Name(QName(..), readName)
import Lexer.Token(Token(..), TokenType(..), isId, isDot, isModule)
import Lexer.Lexer(tokenize)

readSource :: FilePath -> IO (Either Error [Token])
readSource filename = do
  result <- readDep S.empty S.empty Nothing filename
  case result of
    Left  e         -> return $ Left e
    Right (_, toks) -> return $ Right toks

type Files = S.Set FilePath

readDep :: Files -> Files -> Maybe QName -> FilePath
        -> IO (Either Error (Files, [Token]))
readDep codependencies _ _ filename
  | S.member filename codependencies =
      return $ Left (errorAtUnknown
                     ReaderErrorCyclicDependencies
                     ("Cyclic dependencies: " ++ show codependencies))
readDep _ visited _ filename
   | S.member filename visited = return $ Right (visited, [])
readDep codependencies visited mQName filename = do
    fileExists <- doesFileExist filename
    if not fileExists
     then return $ Left (errorAtUnknown
                           ReaderErrorMissingFile
                           ("File does not exist: " ++ show filename))
     else do
      source <- readFile filename
      case tokenize filename source of
        Left  e -> return $ Left e
        Right moduleTokens -> do
          doCheck <- checkModuleNameMatches mQName filename moduleTokens
          doRead  <- readDeps codependencies' visited'
                              (collectDependencies moduleTokens)
          return $ do
            doCheck
            (visited1, depTokens) <- doRead
            return $ (visited1, depTokens ++ moduleTokens)
  where
    codependencies' = S.insert filename codependencies
    visited'        = S.insert filename visited

readDeps :: Files -> Files -> [QName] -> IO (Either Error (Files, [Token]))
readDeps codependencies visited0 [] =
  return $ Right (visited0, [])
readDeps codependencies visited0 (q : qs) = do
  res1 <- readDep codependencies visited0 (Just q) (qnameToFilename q)
  case res1 of
    Left e                  -> return $ Left e
    Right (visited1, toks1) -> do
      res2 <- readDeps codependencies visited1 qs
      case res2 of
        Left e                  -> return $ Left e
        Right (visited2, toks2) -> return $ Right (visited2, toks1 ++ toks2)

nufloExtension :: String
nufloExtension = "nu"

qnameToFilename :: QName -> FilePath
qnameToFilename (Name id)          = id ++ "." ++ nufloExtension
qnameToFilename (Qualified q name) = q ++ "/" ++ qnameToFilename name

checkModuleNameMatches :: Maybe QName -> FilePath -> [Token]
                       -> IO (Either Error ())
checkModuleNameMatches Nothing      filename _    = return $ Right ()
checkModuleNameMatches (Just qname) filename toks
  | null toks || not (isModule (head toks))
              = missingName (getPosition toks)
  | otherwise =
      case readQName (tail toks) of
        Nothing          -> missingName (getPosition toks)
        Just (qname', _) ->
          if qname == qname'
           then return $ Right ()
           else return $
                  Left (Error ReaderErrorModuleNameMismatch
                              (getPosition toks)
                              ("Module name mismatch.\n" ++
                               "Expected: \"" ++ show qname ++ "\"\n" ++
                               "Got     : \"" ++ show qname' ++ "\"\n"))
  where
    getPosition []      = unknownPosition
    getPosition (t : _) = tokenStartPos t
    missingName pos =
      return $
        Left (Error ReaderErrorModuleNameMismatch
                    pos
                    ("File \"" ++ filename ++ "\" misses module name.\n" ++
                     "Expected module name: \"" ++ show qname ++ "\"."))

collectDependencies :: [Token] -> [QName]
collectDependencies []                        = []
collectDependencies (Token _ _ T_Import : ts) =
  case readQName ts of
    Just (qname, ts') -> qname : collectDependencies ts'
    Nothing           -> collectDependencies ts
collectDependencies (_ : ts) = collectDependencies ts

readQName :: [Token] -> Maybe (QName, [Token])
readQName (Token _ _ (T_Id s) : Token _ _ T_Dot : t : rest)
  | isId t    = do (q, rest') <- readQName (t : rest)
                   return (Qualified s q, rest')
readQName (Token _ _ (T_Id s) : ts)
  | null ts || not (isDot (head ts))
              = return (readName s, ts)
readQName _   = Nothing

