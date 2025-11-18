module Syntax.Position(
         Position, line, column, unknown, start, afterChar, after
       ) where

data Position = Position {
                  filename :: String
                , line     :: Integer
                , column   :: Integer
                }

instance Show Position where
  show pos = filename pos ++ ":" ++ show (line pos) ++ ":" ++ show (column pos)

unknown :: Position
unknown = Position "(unknown)" 1 1

start :: String -> Position
start fn = Position fn 1 1

afterChar :: Char -> Position -> Position
afterChar '\n' pos = pos { line = line pos + 1, column = 1 }
afterChar _    pos = pos { column = column pos + 1 }

after :: String -> Position -> Position
after s pos = foldr afterChar pos s

