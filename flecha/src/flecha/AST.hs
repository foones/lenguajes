
module AST(
  Id(..), Program(..), Definition(..), Expr(..), CaseBranch(..),
  ToJSON(..)
) where

type Id = String

data Program = Program [Definition]

data Definition = Def Id Expr
  deriving Show

data Expr = ExprVar Id
          | ExprConstructor Id
          | ExprNumber Int
          | ExprChar Int
          | ExprCase Expr [CaseBranch]
          | ExprLet Id Expr Expr
          | ExprLambda Id Expr
          | ExprApply Expr Expr
  deriving Show

data CaseBranch = CaseBranch Id [Id] Expr
  deriving Show

----

indent :: Int -> String -> String
indent n xs = unlines (map (\ x -> take n (repeat ' ') ++ x) (lines xs))

joinS :: [a] -> [[a]] -> [a]
joinS sep []       = []
joinS sep [x]      = x
joinS sep (x : xs) = x ++ sep ++ joinS sep xs

showParams :: [String] -> String
showParams xs = "[" ++ joinS ", " (map show xs) ++ "]"

data JSON = JI Int | JS String | JN Int [JSON]

instance Show JSON where
  show (JI n) = show n
  show (JS s) = show s
  show (JN n js) =
      "["
      ++ joinS ", " (map show js1)
      ++ (if null js1 || null js2
           then ""
           else ",")
      ++ (if null js2
          then ""
          else "\n" ++ indent 1 (joinS ",\n" (map show js2)))
      ++ "]"
    where js1 = take n js
          js2 = drop n js

class ToJSON a where
  toJSON :: a -> JSON

instance ToJSON Program where
  toJSON (Program defs) = JN 0 (map toJSON defs)

instance ToJSON Definition where
  toJSON (Def x e) = JN 2 [JS "Def", JS x, toJSON e]

instance ToJSON Expr where
  toJSON (ExprVar x)            = JN 2 [JS "ExprVar", JS x]
  toJSON (ExprConstructor x)    = JN 2 [JS "ExprConstructor", JS x]
  toJSON (ExprNumber x)         = JN 2 [JS "ExprNumber", JI x]
  toJSON (ExprChar x)           = JN 2 [JS "ExprChar", JI x]
  toJSON (ExprCase x branches)  = JN 1 [JS "ExprCase", toJSON x,
                                         JN 0 (map toJSON branches)]
  toJSON (ExprLet x y z)        = JN 2 [JS "ExprLet", JS x,
                                         toJSON y,
                                         toJSON z]
  toJSON (ExprLambda x y)       = JN 2 [JS "ExprLambda", JS x, toJSON y]
  toJSON (ExprApply x y)        = JN 1 [JS "ExprApply", toJSON x, toJSON y]

instance ToJSON CaseBranch where
  toJSON (CaseBranch x ids expr) =
    JN 3 [JS "CaseBranch", JS x, JN (length ids) (map JS ids), toJSON expr]

