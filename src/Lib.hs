module Lib (runTest) where

import Parser (parseFiles)
import System.Environment (getArgs)
import Text.Parsec (runParser)
import Typeck (toplevelTypeEnv)
import Codegen (codegen)

runTest :: IO ()
runTest = do
  filenames <- getArgs
  parsed <- parseFiles filenames
  case parsed of
    Left err -> putStrLn err
    Right asts -> do
      -- print $ toplevelTypeEnv asts
      -- print $ codegen asts
      case codegen asts of
        Left err -> putStrLn err
        Right succ -> putStr succ
