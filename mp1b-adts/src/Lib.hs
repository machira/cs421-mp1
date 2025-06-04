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
list2cons (x : xs) = (Cons x (list2cons xs))

--- ### cons2list
-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x y) = (x : cons2list y)


--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x) = x
eval (PlusExp []) = 0
eval (PlusExp x) = foldr (+) 0 (map eval x)
eval (MultExp []) = 1
eval (MultExp x) = foldr (*) 1 (map eval x)

--- ### list2cons'
list2cons' :: [a] -> List a
list2cons' x = foldr Cons Nil x

--- ### BinTree

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf
  deriving (Show, Eq)

-- BinTree
--- ### sumTree
-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree (Leaf) = 0
sumTree (Node a b c) = a + (sumTree b) + (sumTree c)

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer | BoolVal Bool | StrVal String | ExnVal String
  deriving (Show)

--- ### liftIntOp
-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal (op x y)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
