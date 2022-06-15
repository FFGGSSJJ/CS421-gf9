-- Modify by Guanshujie Fu
-- Completed Date: Feb. 20 2022 15:00
-- MP2


module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) =
    if (op 6 2 == div 6 2 || op 13 3 == mod 13 3) && y == 0
        then ExnVal "Division by 0"
    else IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
    case H.lookup s env of
        Just val -> val
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op intOps
    in  liftIntOp f v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    let b1 = eval e1 env
        b2 = eval e2 env
        Just k = H.lookup op boolOps
    in  liftBoolOp k b1 b2

eval (CompOpExp op e1 e2) env = 
    let b1 = eval e1 env
        b2 = eval e2 env
        Just k = H.lookup op compOps
    in  liftCompOp k b1 b2

--- ### If Expressions


eval (IfExp e1 e2 e3) env = 
    let boolep = eval e1 env
        ep1 = eval e2 env
        ep2 = eval e3 env
    in case boolep of
        (BoolVal True) -> ep1
        (BoolVal False) -> ep2
        _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

    
-- e1 is the function body with type FunExp, which contains a parameter list and a function
-- args are the values given to parameters of e1, 
-- env stores the value of variables used in Function e1
-- After eval e1, params are the parameters function accepted, body is the function FunExp, func_env is just env
-- params_val is a list stores the value of parameters, env does not contain the param value yet
-- env2 stores the variables value used in the function and the value of paras
-- eval (AppExp (FunExp ["a"] (AppExp (VarExp "k") [VarExp "a"])) [BoolExp False]) testenv2
eval (AppExp e1 args) env = 
    case eval e1 env of
        (CloVal params body temp_env) -> calFunc (AppExp e1 args) env
            where 
                calFunc :: Exp -> Env -> Val
                calFunc (AppExp e1 args) env =                  
                    let (CloVal params body func_env) = eval e1 env
                        params_val = [eval m env| m <- args]  
                        env_apd = H.union (H.fromList (zip params params_val)) func_env
                    in eval body env_apd
                calFunc _ _ = ExnVal "Apply to non-closure"
        _ -> ExnVal "Apply to non-closure"
                                                        



--- ### Let Expressions
-- pairs stores the Name - Value, add it into env
-- body stores var to be eval
eval (LetExp pairs body) env = 
    let pairs_env = zip [fst name | name <- pairs ] [eval (snd val) env | val <- pairs]
        env_apd = H.union (H.fromList pairs_env) env 
    in eval body env_apd

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, env_apd)
    where env_apd = H.union (H.fromList [(var, eval e env)]) env

--- ### Sequencing

exec (SeqStmt stmts) penv env = 
    case stmts of
        [] -> ("", penv, env)
        (s1:ss) -> 
            let (pi, penvi, envi) = exec s1 penv env
                (p', penv', env') = exec (SeqStmt ss) penvi envi
            in (pi ++ p', penv', env')
              
    

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = 
    case eval e1 env of
        (BoolVal True) -> exec s1 penv env
        (BoolVal False) -> exec s2 penv env
        _ -> ("exn: Condition is not a Bool", penv, env) 

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", penv_apd, env)
    where penv_apd = H.union (H.fromList [(name, p)]) penv


exec (CallStmt name args) penv env = 
    case H.lookup name penv of
        Just p@(ProcedureStmt f ps body) -> exec body penv env'
            where
                arg_val = [eval var env | var <- args] 
                env' = H.union (H.fromList (zip ps arg_val)) env
        Nothing -> ("Procedure f undefined", penv, env)


