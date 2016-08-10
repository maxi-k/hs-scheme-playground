module Main where
import System.Environment
import Reader
import Evaluator (eval)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
