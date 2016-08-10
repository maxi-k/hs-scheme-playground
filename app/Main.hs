module Main where
import System.Environment
import LispData
import Reader
import Evaluator (eval)

main :: IO ()
main = getArgs >>= print . rep . head

rep :: String -> LispVal
rep = eval . readExpr
