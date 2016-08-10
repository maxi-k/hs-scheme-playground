module Evaluator where
import LispData

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(Atom _) = val
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Decimal _) = val
eval val@(Character _) = val
eval val@(Bool _) = val
eval val@(List _) = val
eval val@(DottedList _ _) = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                             if null parsed
                             then 0
                             else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
