module LispData where

data LispVal = Atom String
  | List [ LispVal ]
  | DottedList [ LispVal ] LispVal
  | Number Integer
  | Decimal Float
  | String String
  | Character String
  | Bool Bool
  deriving Show
