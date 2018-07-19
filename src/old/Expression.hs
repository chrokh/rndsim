{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Expression
  ( Exp (Value, Operation)
  , base
  , eval
  ) where


----------------------------------------------
  -- Expressions
----------------------------------------------

-- An expression is a potentially infinite tree-like structure that only grows
-- in one direction. It can usefully be thought of as a non-empty list where
-- each element is comprised of three things. An operator, a value, and the
-- rest of the list. Operators are binary but does not necessarily operate on
-- the same type as the type of the value. The intention is that the operator
-- should operate on the codomain of the domain of the values.

type Operator a = a -> a -> a
data Exp a b = Value a
             | Operation (Exp a b) (Operator b) a


-- The base of an expression can trivially be extracted by recursively digging
-- down to the initial value.

base :: Exp a b -> a
base (Value x)            = x
base (Operation expr _ _) = base expr


-- An expression can be evaluated by providing a function that maps from the
-- domain of the values to the codomain of the operators.

eval :: (a -> b) -> (Exp a b) -> b
eval f (Value x)             = f x
eval f (Operation expr op x) = (eval f expr) `op` (f x)

