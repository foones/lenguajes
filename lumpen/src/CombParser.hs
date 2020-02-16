module CombParser((/\), (\/), (/\\), (//\), (/:\), (\./),
                  Parser, manyP, many1P, manyPred, many1Pred,
                  checkEOF, wrap, matchWord, runParser) where

import Control.Applicative
import Control.Monad(void)

data ParseError = ParseError String
  deriving Show

data Parser a b = PP ([a] -> Either ParseError ([a], b))

parseError = Left . ParseError

instance Functor (Parser a) where
  fmap f (PP g) = PP (\ input ->
                        case g input of
                          Left err -> Left err
                          Right (rem, res) -> Right (rem, f res))

instance Applicative (Parser a) where
  pure x = PP (\ input -> Right (input, x))
  PP f <*> PP x = PP (\ input ->
                        case f input of
                          Left err -> Left err
                          Right (input2, ff) ->
                            case x input2 of
                              Left err -> Left err
                              Right (rem, xx) -> Right (rem, ff xx))

instance Monad (Parser a) where
  fail msg = PP (\ _ -> parseError msg)
  return = pure
  PP f >>= g = PP (\ input ->
                 case f input of
                   Left err -> Left err
                   Right (rem, res) ->
                     case g res of
                       PP h -> h rem)

checkPred :: Show a => (a -> Bool) -> Parser a a
checkPred pred = PP (\ input -> case input of
                                  x:xs -> if pred x
                                           then return (x:xs, x)
                                           else parseError ("found " ++ show x)
                                  []   -> parseError "found premature EOF")

skip1 :: Parser a a
skip1 = PP (\ input -> case input of
                         x:xs -> return (xs, x)
                         []   -> parseError "found premature EOF")

checkEOF :: Show a => Parser a ()
checkEOF = PP (\ input -> case input of
                            x:_ -> parseError ("expected EOF, found " ++ show x)
                            []  -> return ([], ()))

negP :: Parser a b -> Parser a ()
negP (PP f) = PP (\ input -> case f input of
                                Left err -> return (input, ())
                                Right _  -> parseError "no match")

infixl 7 /\
infixl 7 \/
infixl 7 /\\
infixl 7 //\
infixl 7 /:\
infixl 7 \./

(/\) :: Parser a b1 -> Parser a b2 -> Parser a (b1, b2)
p1 /\ p2 = do
  r1 <- p1
  r2 <- p2
  return (r1, r2)

(//\) :: Parser a b1 -> Parser a b2 -> Parser a b1
p1 //\ p2 = fst <$> (p1 /\ p2)

(/\\) :: Parser a b1 -> Parser a b2 -> Parser a b2
p1 /\\ p2 = snd <$> (p1 /\ p2)

(/:\) :: Parser a b -> Parser a [b] -> Parser a [b]
p1 /:\ p2 = uncurry (:) <$> (p1 /\ p2)

(\/) :: Parser a b1 -> Parser a b2 -> Parser a (Either b1 b2)
PP f \/ PP g = PP (\ input -> (Left `m` f input) `alt` (Right `m` g input))
  where m = fmap . fmap
        alt (Left _)  y = y
        alt (Right x) _ = Right x

(\./) :: Parser a b -> Parser a b -> Parser a b
p1 \./ p2 = either id id <$> (p1 \/ p2)

seqP :: [Parser a b] -> Parser a [b]
seqP = foldr (/:\) (return [])

matchPred :: Show a => (a -> Bool) -> Parser a a
matchPred p = checkPred p /\\ skip1

matchLit :: (Eq a, Show a) => a -> Parser a a
matchLit = matchPred . (==)

matchWord :: (Eq a, Show a) => [a] -> Parser a ()
matchWord = void . seqP . map matchLit

manyP :: Show a => Parser a b -> Parser a [b]
manyP p = (p /:\ manyP p) \./ stop
  where stop = do negP p \/ checkEOF
                  return []

many1P :: Show a => Parser a b -> Parser a [b]
many1P p = p /:\ manyP p

runParser :: Parser a b -> [a] -> b
runParser (PP f) xs = case f xs of
                        Left err -> error (show err)
                        Right (_, result) -> result

wrap :: Parser a b -> Parser a c -> Parser a c
wrap p q = p /\\ q //\ p

manyPred :: Show a => (a -> Bool) -> Parser a [a]
manyPred  = manyP . matchPred

many1Pred :: Show a => (a -> Bool) -> Parser a [a]
many1Pred = many1P . matchPred

