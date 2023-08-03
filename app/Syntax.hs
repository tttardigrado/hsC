module Syntax where

type Ident = String

data Type
  = IntT             -- int
  | BoolT            -- bool
  | VoidT            -- void
  | FunT [Type] Type -- (t, t, ...) -> t
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod | SL | SR
  | Leq | Geq | Lt  | Gt
  | Eq  | Neq
  | And | Or
  deriving (Show, Eq)

isRelOp :: BinOp -> Bool
isRelOp = (`elem` [Leq, Geq, Lt, Gt, Eq, Neq])

data UnrOp
  = Not | Neg | Print
  deriving (Show, Eq)

data Expr
  = Var  Ident            -- variable
  | Bool Bool             -- boolean
  | Int  Int              -- integer
  | BOp  BinOp Expr Expr  -- exp bop exp
  | UOp  UnrOp Expr       -- uop exp
  | EIf  Expr  Expr Expr  -- exp ? exp : exp
  | Call Ident [Expr]     -- var(exp, exp, exp, ...)
  deriving (Show)

data Stmt
  = Break                        -- break;
  | Continue                     -- continue;
  | ExpStm Expr                  -- exp;
  | While  Expr Stmt             -- while (exp) stm
  | For    Ident Expr Expr Stmt  -- for (var; exp; exp) stm
  | If     Expr Stmt Stmt        -- if (exp) stm else stm
  | Let    Ident Type Expr       -- let var: typ = exp;
  | Set    Ident Expr            -- var = exp;
  | Return Expr                  -- return exp;
  | Blk    [Stmt]                -- { stm; stm; ... }
  deriving Show

data Fun = Fun Ident [Arg] Type Stmt
  deriving Show -- fun f(a1: t1, a2:t2, ...): ty = stm

type Arg = (Ident, Type) -- arg: typ

type Program = [Fun] -- a program is a list of functions