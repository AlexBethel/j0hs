module Lib (runTest) where

import Parser (parseFiles)
import System.Environment (getArgs)
import Text.Parsec (runParser)
import Typeck (toplevelTypeEnv)

runTest :: IO ()
runTest = do
  filenames <- getArgs
  parsed <- parseFiles filenames
  case parsed of
    Left err -> putStrLn err
    Right asts -> print $ toplevelTypeEnv asts
