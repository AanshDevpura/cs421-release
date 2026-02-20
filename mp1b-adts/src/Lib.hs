--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

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
list2cons :: [a] -> List a
list2cons []     = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list
cons2list :: List a -> [a]
cons2list Nil         = []
cons2list (Cons x xs) = x : cons2list xs

--- ### eval
eval :: Exp -> Integer
eval (IntExp n)  = n
eval (PlusExp exps) = sum (map eval exps)
eval (MultExp exps) = product (map eval exps)

--- ### list2cons'
list2cons' :: [a] -> List a
list2cons' = foldr Cons Nil

--- ### BinTree
data BinTree a = Leaf
               | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

--- ### sumTree
sumTree :: Num a => BinTree a -> a
sumTree Leaf           = 0
sumTree (Node v l r) = v + sumTree l + sumTree r

--- ### SimpVal
data SimpVal = IntVal Integer
             | BoolVal Bool
             | StrVal String
             | ExnVal String
  deriving (Show, Eq)

--- ### liftIntOp
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal a) (IntVal b) = IntVal (op a b)
liftIntOp _ _ _                   = ExnVal "not an IntVal!"