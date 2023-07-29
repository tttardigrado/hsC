module TypeCheck where
import Syntax

type Res a = Either String a

type Env = [(String, Type)]


-- checks if the expression [[exp]] has type [[ty]] inside the environment [[env]]
checkExpr :: Env -> Type -> Expr -> Res ()
checkExpr env ty exp = do
    ty' <- typeOfExpr env exp
    if ty == ty' then Right () else Left "Type Error"


-- deduce the type of the expression [[exp]] inside the environment [[env]]
typeOfExpr :: Env -> Expr -> Res Type
typeOfExpr env exp = case exp of
  Int n  -> Right IntT
  Bool b -> Right BoolT
  Var x  -> case lookup x env of
    Just ty -> Right ty
    Nothing -> Left "variable not found"
  
  BOp op e1 e2 | op `elem` [Add,Sub,Mul,Div,Mod,SL,SR] -> do
    checkExpr env IntT e1 
    checkExpr env IntT e2
    Right IntT -- int op int -> int
  BOp op e1 e2 | op `elem` [And,Or] -> do
    checkExpr env BoolT e1 
    checkExpr env BoolT e2
    Right BoolT -- bool op bool -> bool
  BOp op e1 e2 | op `elem` [Lt,Gt,Leq,Geq] -> do
    checkExpr env IntT e1 
    checkExpr env IntT e2
    Right BoolT -- int op int -> bool
  BOp op e1 e2 | op `elem` [Eq,Neq] -> do
    ty' <- typeOfExpr env e1
    checkExpr env ty' e2
    Right BoolT -- a op a -> bool
  
  UOp Neg ex -> do
    checkExpr env IntT ex 
    Right IntT -- -int -> int
  UOp Not ex -> do
    checkExpr env BoolT ex 
    Right BoolT -- !bool -> bool
  UOp Print ex -> do 
    _ <- typeOfExpr env ex
    Right VoidT -- print a -> void
  
  EIf cnd e1 e2 -> do
    checkExpr env BoolT cnd
    ty' <- typeOfExpr env e1
    checkExpr env ty' e2
    Right ty' -- bool ? a : a -> a
  
  Call fun exs -> case lookup fun env of
    Just (FunT argTys ty) -> do
      expTys <- mapM (typeOfExpr env) exs
      if length expTys == length argTys && and (zipWith (==) expTys argTys)
      then Right ty else Left "Arguments error"
    Nothing -> Left "Function used before being defined"
    _       -> Left "Not a function"


-- check if a statement [[stm]] is well-typed inside the environment [[env]]
-- and return statements return rty
checkStmt :: Env -> Maybe Type -> Stmt -> Res Env
checkStmt env rty stm = case stm of
  Break -> Right env
  Continue -> Right env
  ExpStm ex -> do 
    checkExpr env VoidT ex
    Right env

  While ex st -> do
    checkExpr env BoolT ex
    checkStmt env rty st
    Right env

  For i strt end st -> do
    checkExpr env IntT strt
    checkExpr env IntT end
    checkStmt ((i, IntT) : env) rty st
    Right env

  If ex e1 e2 -> do
    checkExpr env BoolT ex
    checkStmt env rty e1
    checkStmt env rty e2
    Right env

  Let var ty exp -> case lookup var env of
    Just  _ -> Left "Variable redefinition"
    Nothing -> do
      checkExpr env ty exp
      Right $ (var,ty) : env

  Set var ex -> case lookup var env of
    Nothing -> Left "Variable not defined"
    Just ty -> do
      checkExpr env ty ex
      Right env
  
  Func f args ty st -> case lookup f env of
    Just  _ -> Left "Function redefinition"
    Nothing -> do
      let ft = FunT (map snd args) ty
      checkStmt ((f, ft) : args ++ env) (Just ty) st
      Right $ (f, ty) : env

  Return exp -> case rty of
    Nothing -> Left "Return used outside of a function"
    Just ty -> do 
      checkExpr env ty exp
      Right env
  
  Blk [] -> Right env
  Blk (st:sts) -> do
    env' <- checkStmt env rty st
    checkStmt env' rty $ Blk sts
    Right env