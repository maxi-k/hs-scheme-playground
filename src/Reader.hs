module Reader
  (
    readExpr
  ) where
import Parser
import LispData
import Text.ParserCombinators.Parsec (parse)

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "Not found: " ++ show err
                   Right val -> val
