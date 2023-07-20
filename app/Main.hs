module Main where

import Parser ( parser )
import Lexer ( scanTokens )
import TypeChecker ( checkStmt )

main :: IO ()
main = do
  s <- getContents
  let tok = scanTokens s
  let ast = parser tok
  let err = checkStmt [] ast
  print tok
  print ast
  print err
