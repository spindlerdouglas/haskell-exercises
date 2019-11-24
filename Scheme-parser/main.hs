module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO()
main = do
    (arg1:arg2) <- getArgs
    let num1 = read arg1 :: Int
    let num2 = read arg2 :: Int
    let result = show (num1 + num2)
    putStrLn("Hello, " ++ result)