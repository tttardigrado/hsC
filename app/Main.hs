module Main where

import Parser
import Lexer

main :: IO ()
main = do
  s <- getContents
  let tok = scanTokens s
  let ast = parser tok
  print tok
  print ast
