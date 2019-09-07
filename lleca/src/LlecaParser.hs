module LlecaParser(
  Grammar(..), GrammarRule(..), GrammarProduction(..),
  Symbol(..), Term(..), Terminal(..), Nonterm(..), parseLleca
) where

import Lexer(Token(..), TokenizerConfig(..), tokenize)

llecaKeywords = [
  "_",
  "ID",
  "STRING",
  "NUM"
 ]

llecaSymbols = [
  "|",
  "=>",
  "[",
  "]",
  "$",
  "(",
  ")",
  ","
 ]

joinS :: [a] -> [[a]] -> [a]
joinS sep []       = []
joinS sep [x]      = x
joinS sep (x : xs) = x ++ sep ++ joinS sep xs

type Nonterm = String
type Terminal = String

data Grammar = Grammar [GrammarRule]

data GrammarRule = GrammarRule Nonterm [GrammarProduction]

data GrammarProduction = GrammarProduction [Symbol] Term

data Symbol = SymbolId
            | SymbolString
            | SymbolNum
            | SymbolLiteral Terminal
            | SymbolNonterm Nonterm
            | SymbolEof -- to compute follow sets
  deriving (Eq, Ord)

data Term = TermHole
          | TermNode String [Term]
          | TermString String
          | TermNum Integer
          | TermParameter Integer (Maybe Term)

type Parser a = [Token] -> (a, [Token])

parseLleca :: String -> Grammar
parseLleca = fst .
             parseGrammar .
             tokenize (TokenizerConfig llecaKeywords llecaSymbols)

parseGrammar :: Parser Grammar
parseGrammar [] = (Grammar [], [])
parseGrammar toks0 =
  let (rule, toks1)    = parseGrammarRule toks0
      (Grammar grammar, toks2) = parseGrammar toks1
   in (Grammar (rule : grammar), toks2)

parseGrammarRule :: Parser GrammarRule
parseGrammarRule toks0 =
  let (id, toks1)          = parseId toks0
      (productions, toks2) = parseProductions toks1
   in (GrammarRule id productions, toks2)

parseId :: Parser String
parseId (T_Id id : toks) = (id, toks)
parseId toks = error ("Expected identifier, found " ++ showTokens toks)

parseProductions :: Parser [GrammarProduction]
parseProductions toks0@(T_Literal "|" : _) =
  let (production, toks1) = parseProduction toks0
      (productions, toks2) = parseProductions toks1
   in (production : productions, toks2) 
parseProductions toks                      = ([], toks)

parseProduction :: Parser GrammarProduction
parseProduction (T_Literal "|" : toks0) =
  let (expansion, toks1) = parseExpansion toks0
      ((), toks2)        = parseArrow toks1
      (term, toks3)      = parseTerm toks2
   in (GrammarProduction expansion term, toks3)

parseArrow :: Parser ()
parseArrow (T_Literal "=>" : toks) = ((), toks)
parseArrow toks = error ("Expected \"=>\", found " ++ showTokens toks)

parseExpansion :: Parser [Symbol]
parseExpansion (T_Literal "ID" : toks0) =
  let (e, toks1) = parseExpansion toks0
   in (SymbolId : e, toks1)
parseExpansion (T_Literal "STRING" : toks0) =
  let (e, toks1) = parseExpansion toks0
   in (SymbolString : e, toks1)
parseExpansion (T_Literal "NUM" : toks0) =
  let (e, toks1) = parseExpansion toks0
   in (SymbolNum : e, toks1)
parseExpansion (T_String string : toks0) =
  let (e, toks1) = parseExpansion toks0
   in (SymbolLiteral string : e, toks1)
parseExpansion (T_Id id : toks0) =
  let (e, toks1) = parseExpansion toks0
   in (SymbolNonterm id : e, toks1)
parseExpansion toks = ([], toks)

parseTerm :: Parser Term
parseTerm (T_Literal "_" : toks) = (TermHole, toks)
parseTerm (T_Id id : toks0) =
  let (arguments, toks1) = parseArguments toks0 in
    (TermNode id arguments, toks1)
parseTerm (T_String string : toks) = (TermString string, toks)
parseTerm (T_Num number : toks)    = (TermNum number, toks)
parseTerm (T_Literal "$" : toks0)  =
  let (number, toks1) = parseNum toks0 in
    case toks1 of
      (T_Literal "[" : toks2) ->
        let (term, toks3) = parseTerm toks2
            ((), toks4)   = parseRbrace toks3
         in (TermParameter number (Just term), toks4)
      _ -> (TermParameter number Nothing, toks1)

parseArguments :: Parser [Term]
parseArguments (T_Literal "(" : toks0) =
  let (arglist, toks1) = parseArglist toks0
      ((), toks2)      = parseRparen toks1
   in (arglist, toks2)
parseArguments toks = ([], toks)

parseArglist :: Parser [Term]
parseArglist toks@(T_Literal ")" : _) = ([], toks)
parseArglist toks0 =
  let (term, toks1)     = parseTerm toks0
      (arglistc, toks2) = parseArglistC toks1
   in (term : arglistc, toks2)

parseArglistC :: Parser [Term]
parseArglistC (T_Literal "," : toks0) =
  let (term, toks1)     = parseTerm toks0
      (arglistc, toks2) = parseArglistC toks1
   in (term : arglistc, toks2)
parseArglistC toks = ([], toks)

parseRparen :: Parser ()
parseRparen (T_Literal ")" : toks) = ((), toks)
parseRparen toks = error ("Expected \")\", found " ++ showTokens toks)

parseRbrace :: Parser ()
parseRbrace (T_Literal "]" : toks) = ((), toks)
parseRbrace toks = error ("Expected \"]\", found " ++ showTokens toks)

parseNum :: Parser Integer
parseNum (T_Num number : toks) = (number, toks)
parseNum toks = error ("Expected number, found " ++ showTokens toks)

showTokens :: [Token] -> String
showTokens toks = show (take 5 toks)

--- Printer

instance Show Grammar where
  show (Grammar grammarRules) = joinS "\n\n" (map show grammarRules) ++ "\n"

instance Show GrammarRule where
  show (GrammarRule name productions) =
    name ++ "\n" ++ joinS "\n" (map show productions)

instance Show GrammarProduction where
  show (GrammarProduction symbols term) =
    left ++ dropWhile (== ' ') right
    where
      left  = "| " ++ joinS " " (map show symbols) ++ " => "
      right = showTerm (fromIntegral (length left)) term

instance Show Symbol where
  show SymbolId           = "ID"
  show SymbolString       = "STRING"
  show SymbolNum          = "NUM"
  show (SymbolLiteral s)  = show s
  show (SymbolNonterm id) = id
  show SymbolEof          = "<EOF>"

showTerm :: Integer -> Term -> String
showTerm n TermHole          = indent n ++ "_"
showTerm n (TermNode f [])   = indent n ++ f
showTerm n (TermNode f args) =
  indent n ++ f ++ "(\n" ++
  joinS ",\n" (map (showTerm (n + 2)) args) ++
  "\n" ++ indent n ++ ")"
showTerm n (TermString s)    = indent n ++ show s
showTerm n (TermNum x)       = indent n ++ show x
showTerm n (TermParameter x Nothing)     = indent n ++ "$" ++ show x
showTerm n (TermParameter x (Just term)) =
  indent n ++ "$" ++ show x ++ "[\n" ++
  showTerm (n + 2) term ++
  "\n" ++ indent n ++ "]"

indent :: Integer -> String
indent n = take (fromIntegral n) (repeat ' ')

instance Show Term where
  show term = showTerm 0 term

