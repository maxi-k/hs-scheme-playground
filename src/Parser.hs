module Parser (
  parseExpr
  ) where
import LispData
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readHex, readOct, readDec, readFloat)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseDecimal
            <|> parseNumber
            <|> parseChar
            <|> parseQuoted
            <|> parseListVal

character :: Parser Char
character = (noneOf "\"") <|> escaped
  where escaped = char '\\' >> oneOf "\\\"nrt"

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many character
                 char '"'
                 return $ String x


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
-- decimal = many digit >> char '.' >> many1 digit
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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseListVal :: Parser LispVal
parseListVal = do char '('
                  x <- try parseList <|> parseDottedList
                  char ')'
                  return x
