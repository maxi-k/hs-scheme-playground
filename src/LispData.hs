module LispData where

data LispVal = Atom String
  | List [ LispVal ]
  | DottedList [ LispVal ] LispVal
  | Number Integer
  | Decimal Float
  | String String
  | Character Char
  | Bool Bool
  -- deriving Show

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom a) = a
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number num) = show num
showVal (Decimal dec) = show dec
showVal (Character c) = ['\'', c, '\'']
showVal (Bool False) = "#f"
showVal (Bool True) = "#t"
showVal (List contents) = "(" ++ (showListContents contents) ++ ")"
showVal (DottedList contents end) = "(" ++ (showListContents contents) ++ " . " ++ (showVal end) ++ ")"

showListContents :: [LispVal] -> String
showListContents = unwords . map showVal
