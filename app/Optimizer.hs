module Optimizer where

import Syntax

simplifyOne :: Expr -> Expr
simplifyOne e = case e of
  Aop o   (Int      x) (Int      y) -> Int $ aToOp o x y
  Aop Add e1           (Int      0) -> e1         -- e1 + 0 = e1
  Aop Add (Int      0) e2           -> e2         -- 0 + e2 = e1
  Aop Sub e1           (Int      0) -> e1         -- e1 - 0 = e1
  Aop Sub (Int      0) e2           -> e2         -- 0 - e2 = e1
  Aop Mul _            (Int      0) -> Int 0      -- e1 * 0 = 0
  Aop Mul (Int      0) _            -> Int 0      -- 0 * e2 = 0
  Aop Mul e1           (Int      1) -> e1         -- e1 * 1 = e1
  Aop Mul (Int      1) e2           -> e2         -- 1 * e2 = e2
  
  Bop o   (Bool     x) (Bool     y) -> Bool $ bToOp o x y
  Bop And e1           (Bool  True) -> e1         -- e1 && True = e1
  Bop And (Bool  True) e2           -> e2         -- True && e2 = e2
  Bop And _            (Bool False) -> Bool False -- e1 && False = False
  Bop And (Bool False) _            -> Bool False -- False && e2 = False
  Bop Or  _            (Bool  True) -> Bool True  -- e1 || True = True
  Bop Or  (Bool  True) _            -> Bool True  -- True || e2 = True
  Bop Or  e1           (Bool False) -> e1         -- e1 || False = e1
  Bop Or  (Bool False) e2           -> e2         -- False || e2 = e2

  EIf (Bool  True) e1  _            -> e1
  EIf (Bool False) _   e2           -> e2

  Cop o   (Int      x) (Int      y) -> Bool $ cToOp o x y  
  Not                  (Bool     x) -> Bool $ not x
  exp                               -> exp


simplifyExpr :: Expr -> Expr
simplifyExpr e = case e of
  Aop o e1 e2 -> simplifyOne $ Aop o (simplifyExpr e1) (simplifyExpr e2)
  Cop o e1 e2 -> simplifyOne $ Cop o (simplifyExpr e1) (simplifyExpr e2)
  Bop o e1 e2 -> simplifyOne $ Bop o (simplifyExpr e1) (simplifyExpr e2)
  EIf b e1 e2 -> simplifyOne $ EIf (simplifyExpr b) (simplifyExpr e1) (simplifyExpr e2)
  Not      e1 -> simplifyOne $ Not $ simplifyExpr e1
  exp         -> exp


simplifyBlock :: [Stmt] -> [Stmt]
simplifyBlock sts = case sts of
  []           -> []
  Break    : _ -> [Break]
  Continue : _ -> [Continue]
  x : xs       -> simplifyStmt x : simplifyBlock xs

simplifyStmt :: Stmt -> Stmt
simplifyStmt s = case s of
  Break            -> Break
  Continue         -> Continue
  Let    str ty ex -> Let str ty $ simplifyExpr ex
  Print         ex -> Print $ simplifyExpr ex
  Set       str ex -> Set str $ simplifyExpr ex

  Blk sts -> case simplifyBlock sts of
    [st] -> st -- { s } => s
    sts' -> Blk sts'
  
  For str e1 e2 st -> case (simplifyExpr e1, simplifyExpr e2) of
    (Int x, Int y) | x >= y -> Blk []
    (e1', e2')              -> case simplifyStmt st of
      Blk []   -> Blk []
      Break    -> Blk []
      Continue -> Blk []
      st'      -> For str e1' e2' st'

  While      ex st -> case simplifyExpr ex of
    Bool False -> Blk [] -- while (False) st => {}
    ex'        -> case simplifyStmt st of
      Blk []   -> Blk []
      Break    -> Blk []
      Continue -> Blk []
      st'      -> While ex' st'
  
  If      ex s1 s2 -> case simplifyExpr ex of
    Bool True  -> s1 --if (true)  s1 s2 => s1
    Bool False -> s2 --if (false) s1 s2 => s2
    ex'        -> If ex' s1 s2 