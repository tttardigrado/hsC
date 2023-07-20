module Syntax where

-- Alias for a String as an Identifier
type Ident = String

-- Enum of Arithmetic operations
data AOp = Add | Sub | Mul | Div | Mod
  deriving (Show, Enum, Eq)

-- Enum of Comparisson operations
data COp = Eq | Neq | Lt | Gt | Leq | Geq
  deriving (Show, Enum, Eq)

-- Enum of boolean operations
data BOp = And | Or
  deriving (Show, Enum, Eq)

data Type = TyInt | TyBool
  deriving (Show, Enum, Eq)

-- Integer Expressions
data Expr
  = Var  Ident           -- x
  | Int  Int             -- n
  | Bool Bool            -- T or F
  | Aop  AOp Expr Expr   -- e ∘ e
  | Cop  COp Expr Expr   -- b ⪯ b
  | Bop  BOp Expr Expr   -- e ∘ e
  | Not  Expr            -- !e
  deriving (Show, Eq)

-- Statements
data Stmt
 = Skip                  -- skip
 | Let   Ident Type Expr -- let x: t := e;
 | Print Expr            -- print(i);
 | Set   Ident Expr      -- x := i;
 | While Expr Stmt       -- while (b) s
 | If    Expr Stmt Stmt  -- if (b) s else s
 | Blk   [Stmt]          -- { s; s; ... s; }
 deriving (Show, Eq)