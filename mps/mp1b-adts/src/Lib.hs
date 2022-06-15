--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons a b) = a : cons2list b

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer 
eval (IntExp x) = x
eval (PlusExp xs) = sum (map eval xs)
eval (MultExp xs) = foldr (*) 1 (map eval xs)

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' xs = foldr Cons Nil xs

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf 
      deriving (Show, Eq)

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node a leftT rightT) = a + sumTree leftT + sumTree rightT

--- ### SimpVal

-- SimpVal
data SimpVal =   IntVal Integer
              |  BoolVal Bool
              |  StrVal String
              |  ExnVal String 
      deriving (Show, Eq)

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal a) (IntVal b) = IntVal (op a b)

