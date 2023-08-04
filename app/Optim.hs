module Optim (simplify) where

import Syntax

-- simplify an expression without evaluating it
simplifyExpr :: Expr -> Expr
simplifyExpr exp = case exp of 
  Var x        -> Var  x
  Bool b       -> Bool b
  Int n        -> Int  n
  BOp op e1 e2 -> simpOne $ BOp op (simplifyExpr e1) (simplifyExpr e2)
  UOp op ex    -> simpOne $ UOp op (simplifyExpr ex)
  EIf e1 e2 e3 -> simpOne $ EIf    (simplifyExpr e1) (simplifyExpr e2) (simplifyExpr e3)
  Call s exs   -> Call s  $ map simplifyExpr exs
  where 
    simpOne exp = case exp of
      EIf (Bool  True) e1 _ -> e1 -- true  ? e1 : e2 => e1
      EIf (Bool False) _ e2 -> e2 -- false ? e1 : e2 => e2

      UOp Neg (Int  n) -> Int  $ negate n
      UOp Not (Bool b) -> Bool $ not b

      BOp Add (Int n) (Int k) -> Int $ n + k
      BOp Add e1      (Int 0) -> e1 -- e1 + 0 => e1
      BOp Add (Int 0) e2      -> e2 -- 0 + e2 => e2

      BOp Sub (Int n) (Int k) -> Int $ n - k
      BOp Sub e1      (Int 0) -> e1         -- e1 - 0 => e1
      BOp Sub (Int 0) e2      -> UOp Neg e2 -- 0 - e2 => -e2

      BOp Mul (Int n) (Int k) -> Int $ n * k
      BOp Mul _       (Int 0) -> Int 0 -- e1 * 0 => 0
      BOp Mul (Int 0) _       -> Int 0 -- 0 * e2 => 0
      BOp Mul e1      (Int 1) -> e1    -- e1 * 1 => e1
      BOp Mul (Int 1) e2      -> e2    -- 1 * e2 => e2

      BOp Div (Int n) (Int k) -> Int $ n `div` k
      BOp Div (Int 0) _       -> Int 0 -- 0 / e2 => 0
      BOp Div e1      (Int 1) -> e1    -- e1 / 1 => e1

      BOp Mod (Int n) (Int k) -> Int $ n `mod` k

      -- SL and SR
      BOp Leq (Int  n) (Int  k) -> Bool $ n <= k
      BOp Geq (Int  n) (Int  k) -> Bool $ n >= k
      BOp Lt  (Int  n) (Int  k) -> Bool $ n <  k
      BOp Gt  (Int  n) (Int  k) -> Bool $ n >  k
      BOp Eq  (Int  n) (Int  k) -> Bool $ n == k
      BOp Neq (Int  n) (Int  k) -> Bool $ n /= k
      BOp Eq  (Bool b) (Bool c) -> Bool $ b == c
      BOp Neq (Bool b) (Bool c) -> Bool $ b /= c

      BOp And (Bool     b) (Bool     c) -> Bool $ b && c
      BOp And (Bool  True) e2           -> e2         -- true  && e2 => e2
      BOp And (Bool False) _            -> Bool False -- false && e2 => false
      BOp And e1           (Bool  True) -> e1         -- e1 &&  true => e1
      BOp And _            (Bool False) -> Bool False -- e1 && false => false
      
      BOp Or  (Bool     b) (Bool     c) -> Bool $ b || c
      BOp Or  (Bool  True) _            -> Bool True -- true  || e2 => true
      BOp Or  (Bool False) e2           -> e2        -- false || e2 => e2
      BOp Or  _            (Bool  True) -> Bool True -- e1 ||  true => true
      BOp Or  e1           (Bool False) -> e1        -- e1 || false => e1

      ex -> ex


-- simplify a block without evaluating it
simplifyBlock :: [Stmt] -> [Stmt]
simplifyBlock sts = case sts of
  []           -> []
  Break    : _ -> [Break]    -- { ... break;    ... } => { ... break;    }
  Continue : _ -> [Continue] -- { ... continue; ... } => { ... continue; }
  Return e : _ -> [Return e] -- { ... return x; ... } => { ... return x; }
  x : xs       -> simplifyStmt x : simplifyBlock xs


-- simplify a statement without evaluating it
simplifyStmt :: Stmt -> Stmt
simplifyStmt s = case s of
  Break            -> Break
  Continue         -> Continue
  ExpStm ex        -> ExpStm $ simplifyExpr ex
  Return ex        -> Return $ simplifyExpr ex
  Let    str ty ex -> Let str ty $ simplifyExpr ex
  Set       str ex -> Set str $ simplifyExpr ex

  Blk sts -> case simplifyBlock sts of
    [st] -> st -- { s } => s
    sts' -> Blk sts'
  
  For str e1 e2 st -> case (simplifyExpr e1, simplifyExpr e2) of
    (Int x, Int y) | x >= y -> Blk []
    (e1', e2')              -> case simplifyStmt st of
      Blk []   -> Blk [] -- for (i; e; e) {}        => {}
      Break    -> Blk [] -- for (e; e; e) break;    => {}
      Continue -> Blk [] -- for (e; e; e) continue; => {}
      st'      -> For str e1' e2' st'

  While      ex st -> case simplifyExpr ex of
    Bool False -> Blk [] -- while (False) st => {}
    ex'        -> case simplifyStmt st of
      Blk []   -> Blk [] -- while (cond) {}        => {}
      Break    -> Blk [] -- while (cond) break;    => {}
      Continue -> Blk [] -- while (cond) continue; => {}
      st'      -> While ex' st'
  
  If      ex s1 s2 -> case simplifyExpr ex of
    Bool True  -> s1 --if (true)  s1 s2 => s1
    Bool False -> s2 --if (false) s1 s2 => s2
    ex'        -> If ex' s1 s2


-- simplify a function without evaluating it
simplifyFun :: Fun -> Fun
simplifyFun (Fun f args ty stm) = Fun f args ty $ simplifyStmt stm


-- simplify a program without evaluating it
simplify :: Program -> Program
simplify = map simplifyFun