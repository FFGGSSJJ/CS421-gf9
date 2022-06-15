--- Given Code
--- ==========

    module Lib where

        import System.IO (hPutStrLn, hPutStr, stdout, hFlush)
        
        import Data.List (intercalate)
        
        import Data.Functor.Identity (Identity)
        import Text.ParserCombinators.Parsec hiding (Parser)
        import Text.Parsec.Prim (ParsecT)
        
        --- The Types
        --- ---------
        
        data Stmt = Decl String [String] Exp
                    deriving (Eq)
        
        instance Show Stmt where
            show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)
        
        data Exp = IntExp Integer
                 | VarExp String
                 | LamExp String Exp
                 | IfExp Exp Exp Exp
                 | OpExp String Exp Exp
                 | AppExp Exp Exp
                 deriving (Eq)
        
        instance Show Exp where
            show (VarExp s)       = s
            show (IntExp i)       = show i
            show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
            show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                                    ++ " else " ++ show e3 ++ ")"
            show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
            show (AppExp f e)     = show f ++ " " ++ show e
        
        ctorShow :: Exp -> String
        ctorShow (VarExp s)       = "VarExp " ++ show s
        ctorShow (IntExp i)       = "IntExp " ++ show i
        ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
        ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                        ++ ctorShow e2 ++ ") ("
                                        ++ ctorShow e3 ++ ")"
        ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                        ++ ctorShow e1 ++ ") ("
                                        ++ ctorShow e2 ++ ")"
        ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"
        
        --- Problems
        --- ========
        
        --- Manual Translation
        --- ------------------
        
        --- ### `factk :: Integer -> (Integer -> t) -> t`
        
        factk :: Integer -> (Integer -> t) -> t
        factk m func = aux m func
            where aux :: Integer -> (Integer -> t) -> t
                  aux 0 newf = newf 1
                  aux x newf = aux (x-1) (\res -> newf (x*res))
        
        --- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`
        
        evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
        evenoddk xx op1 op2 = if even (last xx) then aux xx op1 True else aux xx op2 False
                where aux :: [Integer] -> (Integer -> t) -> Bool -> t
                      aux [] f _ = f 0
                      aux (x:xs) f b = if even x == b then aux xs (\res -> f (x + res)) b else aux xs f b
        
        --- Automated Translation
        --- ---------------------
        
        gensym :: Integer -> (String, Integer)
        gensym i = ("v" ++ show i, i + 1)
        
        --- ### Define `isSimple`
        
        isSimple :: Exp -> Bool
        isSimple (IntExp e) = True
        isSimple (VarExp e) = True
        isSimple (IfExp e1 e2 e3) = (isSimple e1) && (isSimple e2) && (isSimple e3)
        isSimple (OpExp e1 e2 e3) = (isSimple e2) && (isSimple e3)
        isSimple (AppExp e1 e2) = False
        isSimple _ = True
        
        
        --- ### Define `cpsExp` - Overview
        
        cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
        --cpsExp = undefined
        
        --- #### Define `cpsExp` for Integer and Variable Expressions
        cpsExp e@(IntExp i) k num = (AppExp k e, num)
        cpsExp e@(VarExp v) k num = (AppExp k e, num)
        --- #### Define `cpsExp` for Application Expressions
        cpsExp e@(AppExp fun arg) k num = 
            case (isSimple arg) of
                True -> (AppExp e k, num)
                False -> cpsExp arg (LamExp v (AppExp (AppExp fun (VarExp v)) k)) n
                where (v, n) = gensym num
        
        --- #### Define `cpsExp` for Operator Expressions
        cpsExp e@(OpExp op e1 e2) k num 
            | (isSimple e1) && (isSimple e2) == True = (AppExp k e, num)
            | (isSimple e2) == True = cpsExp e1 (LamExp v1 (AppExp k (OpExp op (VarExp v1) e2))) n1
            | (isSimple e1) == True = cpsExp e2 (LamExp v1 (AppExp k (OpExp op e1 (VarExp v1)))) n1
            | otherwise = cpsExp e1 (LamExp v1 e2trans) res
            where (v1, n1) = gensym num 
                  (v2, n2) = gensym n1
                  (e2trans, res) = cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) n2
                
            
        --- #### Define `cpsExp` for If Expressions
        cpsExp (IfExp e1 e2 e3) k num = 
            case isSimple e1 of
                True -> (IfExp e1 ee2 ee3, num)     
                False -> cpsExp e1 (LamExp v (IfExp (VarExp v) tr2 tr3)) n
                where (v, n) = gensym num
                      (ee2, ne2) = cpsExp e2 k num      -- e2, e3 can be non-simple exp. pdf default e2 and e3 as simple
                      (ee3, ne3) = cpsExp e3 k num 
                      (tr2, n2) = cpsExp e2 k n
                      (tr3, n3) = cpsExp e3 k n
        --- ### Define `cpsDecl`
        
        cpsDecl :: Stmt -> Stmt
        cpsDecl (Decl f params body) = (Decl f paramsk bodyk)
            where paramsk = params ++ ["k"]
                  (bodyk, n) = cpsExp body (VarExp "k") 1   -- the number should be 1 but not 0 !!!!
                                                            -- i don't know why
        