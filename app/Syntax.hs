module Syntax where

type Ident = String

data Type
  = IntT
  | BoolT
  | VoidT
  | FunT [Type] Type
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod | SL | SR
  | Leq | Geq | Lt  | Gt
  | Eq  | Neq
  | And | Or
  deriving (Show, Eq)

data UnrOp
  = Not | Neg | Print
  deriving (Show, Eq)

data Expr
  = Var  Ident
  | Bool Bool
  | Int  Int
  | BOp  BinOp Expr Expr
  | UOp  UnrOp Expr
  | EIf  Expr  Expr Expr
  | Call Ident [Expr]
  deriving (Show)

data Stmt
  = Break
  | Continue
  | ExpStm Expr
  | While  Expr Stmt
  | For    Ident Expr Expr Stmt
  | If     Expr Stmt Stmt
  | Let    Ident Type Expr
  | Set    Ident Expr
  | Func   Ident [Arg] Type Stmt
  | Return Expr
  | Blk    [Stmt]
  deriving Show


type Arg = (Ident, Type)