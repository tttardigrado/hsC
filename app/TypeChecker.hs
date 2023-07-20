module TypeChecker where

import Syntax (Expr (..), Stmt (..), Type (..))

type Res a = Either String a

type Ctx a = [(String, a)]

checkExpr :: Ctx Type -> Type -> Expr -> Res ()
checkExpr ctx ty exp = do
  ty' <- typeOfExpr ctx exp
  if ty == ty' then Right () else Left $ concat
    [ "Type Error: The expression "
    , show exp
    , " was expected to have type "
    , show ty
    , " but has type "
    , show ty'
    ]

getVarType :: Ctx Type -> String -> Res Type
getVarType ctx var = case lookup var ctx of
  Nothing -> Left $ concat ["variable ", var, " not found"]
  Just ty -> Right ty

typeOfExpr :: Ctx Type -> Expr -> Res Type
typeOfExpr ctx exp = case exp of
  Var v -> getVarType ctx v
  Int _ -> Right TyInt
  Bool _ -> Right TyBool
  Aop o e1 e2 -> do
    checkExpr ctx TyInt e1
    checkExpr ctx TyInt e2
    Right TyInt
  Cop o e1 e2 -> do
    checkExpr ctx TyInt e1
    checkExpr ctx TyInt e2
    Right TyBool
  Bop o e1 e2 -> do
    checkExpr ctx TyBool e1
    checkExpr ctx TyBool e2
    Right TyBool
  Not e -> do
    checkExpr ctx TyBool e
    Right TyBool

checkStmt :: Ctx Type -> Stmt -> Res (Ctx Type)
checkStmt ctx stm = case stm of
  Let v ty ex -> do
    checkExpr ctx ty ex
    Right ((v, ty) : ctx)
  Print ex -> do
    checkExpr ctx TyInt ex
    Right ctx
  Set v ex -> do
    ty <- getVarType ctx v
    checkExpr ctx ty ex
    Right ctx
  While ex st -> do
    checkExpr ctx TyBool ex
    checkStmt ctx st
    Right ctx
  If ex s1 s2 -> do
    checkExpr ctx TyBool ex
    checkStmt ctx s1
    checkStmt ctx s2
    Right ctx
  Blk [] -> Right ctx
  Blk (x : xs) -> do
    ctx' <- checkStmt ctx x
    checkStmt ctx' $ Blk xs
    Right ctx
