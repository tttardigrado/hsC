module Main where

import Lexer (scanTokens)
import Parser (parser)
import TypeCheck (checkStmt)

main :: IO ()
main = do
  s <- getContents
  let tok = scanTokens s
  let ast = parser tok
  let err = checkStmt [] Nothing ast
  print tok
  print ast
  print err