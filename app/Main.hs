module Main where

import Lexer (scanTokens)
import Parser (parser)
import TypeChecker (checkStmt)
import Optimizer (simplifyStmt)

main :: IO ()
main = do
  s <- getContents
  let tok = scanTokens s
  let ast = parser tok
  let err = checkStmt [] ast
  let opt = simplifyStmt ast
  print tok
  print ast
  print err
  print opt
