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

-- Integer Expressions
data IExpr
  = Var Ident             -- x
  | Int Int               -- n
  | Aop AOp IExpr IExpr   -- i ∘ i
  deriving (Show, Eq)

-- Boolean Expressions
data BExpr
  = Bool Bool             -- T or F
  | Cop COp IExpr IExpr   -- b ⪯ b
  | Bop BOp BExpr BExpr   -- b ∘ b
  | Not BExpr             -- !b
  deriving (Show, Eq)

-- Statements
data Stmt
 = Skip                   -- skip
 | Let   Ident IExpr      -- let x := i;
 | Print IExpr            -- print(i);
 | Set   Ident IExpr      -- x := i;
 | While BExpr Stmt       -- while (b) s
 | If    BExpr Stmt Stmt  -- if (b) s else s
 | Blk   [Stmt]           -- { s; s; ... s; }
 deriving (Show, Eq)