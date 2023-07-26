module TypeChecker where

import Syntax (Expr (..), Stmt (..), Type (..))
import qualified Data.HashMap as M

type Res a = Either String a

type Ctx = M.Map String Type

checkExpr :: Ctx -> Type -> Expr -> Res ()
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

getVarType :: Ctx -> String -> Res Type
getVarType ctx var = case M.lookup var ctx of
  Just ty -> Right ty
  Nothing -> Left $ concat
    [ "Name Error: The variable "
    , var
    , " was referenced before assignment"
    ]


typeOfExpr :: Ctx -> Expr -> Res Type
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
  EIf b e1 e2 -> do
    checkExpr ctx TyBool b
    ty <- typeOfExpr ctx e1
    checkExpr ctx ty e2
    Right ty
  Not e -> do
    checkExpr ctx TyBool e
    Right TyBool


checkStmt :: Ctx -> Stmt -> Res Ctx
checkStmt = check False where
  check inLoop ctx stm = case stm of
    Break -> if inLoop
      then Right ctx 
      else Left "Error: Break Statement occurs outside of a loop."
    
    Continue -> if inLoop 
      then Right ctx 
      else Left "Error: Continue Statement occurs outside of a loop."
    
    Let v ty ex -> do
      if v `elem` M.keys ctx
      then Left $ concat ["Name Error: The variable ", v, " was redifined."]
      else do
        checkExpr ctx ty ex
        Right $ M.insert v ty ctx
    
    Print ex -> do
      checkExpr ctx TyInt ex
      Right ctx
    
    Set v ex -> do
      ty <- getVarType ctx v
      checkExpr ctx ty ex
      Right ctx
    
    While ex st -> do
      checkExpr ctx TyBool ex
      check True ctx st
      Right ctx
    
    For v e1 e2 st -> do
      checkExpr ctx TyInt e1
      checkExpr ctx TyInt e2
      check True (M.insert v TyInt ctx) st
      Right ctx
    
    If ex s1 s2 -> do
      checkExpr ctx TyBool ex
      checkStmt ctx s1
      checkStmt ctx s2
      Right ctx
    
    Blk [] -> Right ctx
    Blk (x : xs) -> do
      ctx' <- check inLoop ctx x
      check inLoop ctx' $ Blk xs
      Right ctx