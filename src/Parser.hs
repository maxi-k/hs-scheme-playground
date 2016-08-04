module Parser (
  spaces,
  symbol,
  readExpr,
  parseString,
  parseAtom ) where
import LispData
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readHex, readOct, readDec, readFloat)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "Not found: " ++ show err
                   Right val -> "Found value " ++ show val

parseExpr = parseAtom <|> parseString <|> parseDecimal <|> parseNumber <|> parseChar

character :: Parser String
character = fmap return (noneOf "\"") <|> escaped
  where escaped = do c <- char '\\'
                     e <- oneOf "\\\"nrt"
                     return [c, e]

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many character
                 char '"'
                 return $ String $ concat x

parseAtom :: Parser LispVal
parseAtom =  do first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = [first] ++ rest
                return $ case atom of
                           "#t" -> Bool True
                           "#f" -> Bool False
                           otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (many1 (char 'x' <|> digit)) >>= (return . Number . parseNumberFn)
            where parseNumberFn = \s -> case s of
                         ('0':'x':rest) -> fst $ head $ readHex rest
                         ('0':rest)     -> fst $ head $ readOct rest
                         otherwise      -> fst $ head $ readDec s

decimal :: Parser String
decimal = try $ do d <- many digit
                   p <- char '.'
                   a <- many1 digit
                   return $ d ++ [p] ++ a


parseDecimal :: Parser LispVal
parseDecimal = decimal >>= (return . Decimal . parseFn)
            where parseFn = fst . head . readFloat

parseChar :: Parser LispVal
parseChar = do char '\\'
               char '#'
               x <- character
               return $ Character x
