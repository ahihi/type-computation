{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Natural number arithmetic

module NatArithmetic where

-- Natural numbers
data Z   -- Zero
data S a -- Successor of a (a = Z or a = S a')
data N a -- Negative numbers (a = Z or a = S a')
         --   N Z = -1, N (S Z) = -2, N (S (S Z)) = -3...

-- Successor
type family Succ a
type instance Succ Z         = S Z
type instance Succ (S a)     = S (S a)
type instance Succ (N Z)     = Z
type instance Succ (N (S a)) = N a

-- Predecessor
type family Pred a
type instance Pred Z     = N Z
type instance Pred (S a) = a
type instance Pred (N a) = N (S a)

-- Addition
type family Add a b
type instance Add a Z     = a
type instance Add a (S b) = Add (Succ a) b
type instance Add a (N b) = Add (Pred a) (Succ (N b))

-- Negation
type family Neg a
type instance Neg Z     = Z
type instance Neg (S a) = N a
type instance Neg (N a) = S a

-- Subtraction
type Sub a b = Add a (Neg b)

-- Multiplication
type family Mul a b
type instance Mul a Z     = Z
type instance Mul a (S b) = Add a (Mul a b)
type instance Mul a (N b) = Neg (Mul a (S b))
