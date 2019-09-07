module Parser where

import qualified Data.Map as M
import Data.List(union)

import Lexer(Token(..), TokenizerConfig(..), tokenize, isIdent)

import LlecaParser(
  Grammar(..), GrammarRule(..), GrammarProduction(..),
  Symbol(..), Term(..), Nonterm(..), Terminal(..), parseLleca
 )

type Errmsg = String

isIdentifier :: String -> Bool
isIdentifier string = all isIdent string

uconcat :: Eq a => [[a]] -> [a]
uconcat = foldr union []

parse :: Grammar -> Nonterm -> Either Errmsg Term
parse grammar input = error ""

grammarKeywords :: Grammar -> [Terminal]
grammarKeywords grammar = kwsGrammar grammar
  where
    kwsGrammar :: Grammar -> [Terminal]
    kwsGrammar (Grammar grammarRules) =
      uconcat (map kwsGrammarRule grammarRules)

    kwsGrammarRule :: GrammarRule -> [Terminal]
    kwsGrammarRule (GrammarRule _ productions) =
      uconcat (map kwsProduction productions)

    kwsProduction (GrammarProduction symbols _) =
      uconcat (map kwsSymbol symbols)

    kwsSymbol (SymbolLiteral s) =
      if isIdentifier s
       then [s]
       else []
    kwsSymbol _ = []

grammarPunctuation :: Grammar -> [Terminal]
grammarPunctuation grammar = punctGrammar grammar
  where
    punctGrammar :: Grammar -> [Terminal]
    punctGrammar (Grammar grammarRules) =
      uconcat (map punctGrammarRule grammarRules)

    punctGrammarRule :: GrammarRule -> [Terminal]
    punctGrammarRule (GrammarRule _ productions) =
      uconcat (map punctProduction productions)

    punctProduction (GrammarProduction symbols _) =
      uconcat (map punctSymbol symbols)

    punctSymbol (SymbolLiteral s) =
      if isIdentifier s
       then []
       else [s]
    punctSymbol _ = []

grammarNonterminals :: Grammar -> [Nonterm]
grammarNonterminals grammar = nontermsGrammar grammar
  where
    nontermsGrammar :: Grammar -> [Nonterm]
    nontermsGrammar (Grammar grammarRules) =
      uconcat (map nontermsGrammarRule grammarRules)

    nontermsGrammarRule :: GrammarRule -> [Nonterm]
    nontermsGrammarRule (GrammarRule nonterminal _) =
      [nonterminal]

productionsFor :: Grammar -> Nonterm -> [GrammarProduction]
productionsFor (Grammar grammarRules) nonterminal = rec grammarRules
  where
    rec :: [GrammarRule] -> [GrammarProduction]
    rec (GrammarRule nonterminal2 productions : rules) = 
      if nonterminal == nonterminal2
       then productions
       else rec rules

allProductions :: Grammar -> [(Nonterm, GrammarProduction)]
allProductions (Grammar grammarRules) = rec grammarRules
  where
    rec :: [GrammarRule] -> [(Nonterm, GrammarProduction)]
    rec [] = []
    rec (GrammarRule nonterminal productions : rules) = 
      map (\ x -> (nonterminal, x)) productions ++ rec rules

type NullableDict = M.Map Nonterm Bool
type FirstDict = M.Map Nonterm [Symbol]
type FollowDict = M.Map Nonterm [Symbol]
type LL1Table = M.Map (Nonterm, Symbol) GrammarProduction

isNullable :: NullableDict -> Nonterm -> Bool
isNullable dict nonterm = M.findWithDefault False nonterm dict

stringIsNullable :: NullableDict -> [Symbol] -> Bool
stringIsNullable dict [] = True
stringIsNullable dict (SymbolNonterm nonterm : rest) =
  isNullable dict nonterm && stringIsNullable dict rest
stringIsNullable _ _ = False

firstSet :: FirstDict -> Nonterm -> [Symbol]
firstSet dict nonterm = M.findWithDefault [] nonterm dict

stringFirstSet :: NullableDict -> FirstDict -> [Symbol] -> [Symbol]
stringFirstSet _ _ [] = []
stringFirstSet nullableDict firstDict (SymbolNonterm a : s) =
  firstSet firstDict a `union`
  if isNullable nullableDict a
   then []
   else stringFirstSet nullableDict firstDict s
stringFirstSet _ _ (sym : _) = [sym]

followSet :: FollowDict -> Nonterm -> [Symbol]
followSet followDict nonterm = M.findWithDefault [] nonterm followDict

startSymbol :: Grammar -> Nonterm
startSymbol grammar =
  case grammar of
    Grammar (GrammarRule nonterminal _ : _) -> nonterminal

---

nullable :: Grammar -> NullableDict
nullable grammar = rec M.empty
  where
    productions :: [(Nonterm, GrammarProduction)]
    productions = allProductions grammar

    rec :: NullableDict -> NullableDict
    rec dict =
        if dict' == dict
         then dict
         else rec dict'
      where
        dict' = foldr (\ k -> M.insert k True) dict newNullables
        newNullables =
            map fst (filter
                     (\ (_, GrammarProduction s _) ->
                        stringIsNullable dict s)
                     productions)

first :: NullableDict -> Grammar -> FirstDict
first nullableDict grammar = rec M.empty
  where
    productions :: [(Nonterm, GrammarProduction)]
    productions = allProductions grammar

    update :: [(Nonterm, GrammarProduction)] -> FirstDict -> FirstDict
    update [] dict = dict
    update ((nonterminal, GrammarProduction s _) : prods) dict =
      update prods $
      M.insert nonterminal
               (firstSet dict nonterminal `union` firstString dict s)
               dict

    firstString :: FirstDict -> [Symbol] -> [Symbol]
    firstString dict [] = []
    firstString dict (SymbolNonterm a : s) =
      M.findWithDefault [] a dict `union`
      (if isNullable nullableDict a
        then firstString dict s
        else [])
    firstString dict (a : _) = [a]

    rec :: FirstDict -> FirstDict
    rec dict =
        if dict == dict'
         then dict
         else rec dict'
      where
        dict' :: M.Map Nonterm [Symbol]
        dict' = update productions dict

follow :: NullableDict -> FirstDict -> Grammar -> FollowDict
follow nullableDict firstDict grammar =
    rec (M.insert (startSymbol grammar) [SymbolEof] M.empty)
  where
    productions :: [(Nonterm, GrammarProduction)]
    productions = allProductions grammar

    grammarNullables :: NullableDict
    grammarNullables = nullable grammar

    update :: [(Nonterm, GrammarProduction)] -> FollowDict -> FollowDict
    update [] dict = dict
    update ((nonterminal, GrammarProduction s _) : prods) dict =
      update prods $
      updateProduction nonterminal s dict

    updateProduction :: Nonterm -> [Symbol] -> FollowDict -> FollowDict
    updateProduction _ [] dict = dict
    updateProduction a (SymbolNonterm b : syms) dict =
        updateProduction a syms $ M.insert b followB dict
      where
        followB = M.findWithDefault [] b dict `union`
                  stringFirstSet nullableDict firstDict syms `union`
                  if stringIsNullable nullableDict syms
                   then M.findWithDefault [] a dict
                   else []
    updateProduction a (_ : syms) dict =
      updateProduction a syms dict
      
    rec :: FollowDict -> FollowDict
    rec dict =
        if dict == dict'
         then dict
         else rec dict'
      where
        dict' :: M.Map Nonterm [Symbol]
        dict' = update productions dict

ll1table :: Grammar -> LL1Table
ll1table grammar = rec productions
  where
    productions :: [(Nonterm, GrammarProduction)]
    productions = allProductions grammar

    nullableDict :: NullableDict
    nullableDict = nullable grammar

    firstDict :: FirstDict
    firstDict = first nullableDict grammar

    followDict :: FollowDict
    followDict = follow nullableDict firstDict grammar
  
    rec :: [(Nonterm, GrammarProduction)] -> LL1Table
    rec [] = M.empty
    rec ((nonterm, production@(GrammarProduction s term)) : prods) =
      let table = rec prods
          directorSymbols =
            stringFirstSet nullableDict firstDict s `union`
            if stringIsNullable nullableDict s
             then followSet followDict nonterm
             else []
       in foldr
            (\ symbol table ->
              if M.member (nonterm, symbol) table
               then error ("Conflict for " ++ show (nonterm, symbol))
               else M.insert (nonterm, symbol) production table)
            table
            directorSymbols

ll1parse :: Grammar -> String -> Term
ll1parse grammar input =
    let (term, toks) =
          parseNonterm
            (startSymbol grammar)
            (tokenize (TokenizerConfig
                        (grammarKeywords grammar)
                        (grammarPunctuation grammar))
                        input)
     in if length toks == 0
         then term
         else error ("Parse error: input remains " ++ show toks)
  where
    table :: LL1Table
    table = ll1table grammar 

    parseNonterm :: Nonterm -> [Token] -> (Term, [Token])
    parseNonterm nonterm toks0 =
        case M.lookup (nonterm, directorSymbol toks0) table of
          Nothing -> error (
                       "Parse error: expected " ++ show nonterm ++ 
                       " but found " ++ show (directorSymbol toks0) ++
                       ".\nNear: " ++ show (take 10 toks0)
                     )
          Just (GrammarProduction symbols action) ->
            let (terms, toks1) = parseSymbols symbols toks0
             in (replaceArgsBy terms action, toks1)
      where
        directorSymbol []                  = SymbolEof
        directorSymbol (T_Id _ : _)        = SymbolId
        directorSymbol (T_String _ : _)    = SymbolString
        directorSymbol (T_Num _ : _)       = SymbolNum
        directorSymbol (T_Literal lit : _) = (SymbolLiteral lit)

    parseSymbols :: [Symbol] -> [Token] -> ([Term], [Token])
    parseSymbols [] toks = ([], toks)
    parseSymbols (SymbolId : syms) (T_Id id : toks0) =
      let (terms, toks1) = parseSymbols syms toks0
       in (TermNode id [] : terms, toks1)
    parseSymbols (SymbolString : syms) (T_String str : toks0) =
      let (terms, toks1) = parseSymbols syms toks0
       in (TermString str : terms, toks1)
    parseSymbols (SymbolNum : syms) (T_Num n : toks0) =
      let (terms, toks1) = parseSymbols syms toks0
       in (TermNum n : terms, toks1)
    parseSymbols (SymbolLiteral k : syms) (T_Literal k' : toks0) | k == k' =
      let (terms, toks1) = parseSymbols syms toks0
       in (TermNode k [] : terms, toks1)
    parseSymbols (SymbolNonterm nonterm : syms) toks0 =
      let (term, toks1)  = parseNonterm nonterm toks0
          (terms, toks2) = parseSymbols syms toks1
       in (term : terms, toks2)
    parseSymbols (sym : _) toks =
      error (
        "Parse error: expected symbol " ++ show sym ++ 
        "near: " ++ show (take 10 toks)
      )

    replaceArgsBy :: [Term] -> Term -> Term
    replaceArgsBy _     TermHole             = TermHole
    replaceArgsBy args (TermNode r xs)       = TermNode
                                                 r
                                                 (map (replaceArgsBy args) xs)
    replaceArgsBy _    (TermString s)        = TermString s
    replaceArgsBy _    (TermNum n)           = TermNum n
    replaceArgsBy args (TermParameter i sub) =
      case sub of
        Nothing   -> args !! (fromIntegral i - 1)
        Just sub' -> replaceHoleBy (replaceArgsBy args sub')
                                   (args !! (fromIntegral i - 1))
 
    replaceHoleBy :: Term -> Term -> Term
    replaceHoleBy term  TermHole             = term
    replaceHoleBy term (TermNode r xs)       = TermNode
                                                 r
                                                 (map (replaceHoleBy term) xs)
    replaceHoleBy _    (TermString s)        = TermString s
    replaceHoleBy _    (TermNum n)           = TermNum n
    replaceHoleBy term (TermParameter i sub) =
      case sub of
        Nothing   -> TermParameter i Nothing
        Just sub' -> TermParameter i (Just (replaceHoleBy term sub'))

