module Syntax where

-- Alias for a String as an Identifier
type Ident = String

-- Enum of Arithmetic operations
data AOp = Add | Sub | Mul | Div | Mod
  deriving (Enum, Eq)

instance Show AOp where
  show = (!!) [" + ", " - ", " * ", " / ", " % "] . fromEnum

precAOp :: AOp -> Int
precAOp = (!!) [7,7,8,8,8] . fromEnum

aToOp :: AOp -> (Int -> Int -> Int)
aToOp = (!!) [(+),(-),(*),div,mod] . fromEnum


-- Enum of Comparisson operations
data COp = Eq | Neq | Lt | Gt | Leq | Geq
  deriving (Enum, Eq)

instance Show COp where
  show = (!!) [" == ", " != ", " < ", " > ", " <= ", ">="] . fromEnum

precCOp :: COp -> Int
precCOp _ = 6

cToOp :: COp -> (Int -> Int -> Bool)
cToOp = (!!) [(==),(/=),(<),(>),(<=),(>=)] . fromEnum

-- Enum of boolean operations
data BOp = And | Or
  deriving (Enum, Eq)

instance Show BOp where
  show = (!!) [" && ", " || "] . fromEnum

precBOp :: BOp -> Int
precBOp _ = 5

bToOp :: BOp -> (Bool -> Bool -> Bool)
bToOp = (!!) [(&&),(||)] . fromEnum

-- Enum of valid Types
data Type = TyInt | TyBool
  deriving (Enum, Eq)

instance Show Type where
  show = (!!) ["int", "bool"] . fromEnum


-- Integer Expressions
data Expr
  = Var  Ident           -- x
  | Int  Int             -- n
  | Bool Bool            -- T or F
  | Aop  AOp Expr Expr   -- e ∘ e
  | Cop  COp Expr Expr   -- b ⪯ b
  | Bop  BOp Expr Expr   -- e ∘ e
  | Not  Expr            -- !e
  deriving (Eq)

instance Show Expr where
  showsPrec p (Var x) = showsPrec p x
  showsPrec p (Int n) = showsPrec p n
  showsPrec p (Bool b) = showsPrec p b
  showsPrec p (Aop o e1 e2) = let q = precAOp o
    in showParen (p >= q) $ showsPrec q e1 . showString (show o) . showsPrec q e2  
  showsPrec p (Cop o e1 e2) = let q = precCOp o
    in showParen (p >= q) $ showsPrec q e1 . showString (show o) . showsPrec q e2  
  showsPrec p (Bop o e1 e2) = let q = precBOp o
    in showParen (p >= q) $ showsPrec q e1 . showString (show o) . showsPrec q e2  
  showsPrec p (Not e) = let q = 9
    in showParen (p >= q) $ showString "!" . showsPrec q e
  
-- Statements
data Stmt
 = Let   Ident Type Expr      -- let x: t = e;
 | Print Expr                 -- print(i);
 | Set   Ident Expr           -- x = i;
 | While Expr Stmt            -- while (b) s
 | For   Ident Expr Expr Stmt -- for (x; e; e) s
 | If    Expr Stmt Stmt       -- if (b) s else s
 | Blk   [Stmt]               -- { s; s; ... s; }
 deriving (Show, Eq)