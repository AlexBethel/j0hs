module Lib (runTest) where

import Parser (parseSourceFile)
import System.Environment (getArgs)
import Text.Parsec (runParser)

runTest :: IO ()
runTest = do
  [filename] <- getArgs
  text <- readFile filename
  let x = runParser parseSourceFile () filename text
  print x
