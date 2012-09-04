{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Integer arithmetic

module IntArithmetic where

-- Integers
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

-- Exponentiation
type family Exp a b
type instance Exp a Z     = S Z
type instance Exp a (S b) = Mul a (Exp a b)

-- For division, we need boolean algebra (I think? lol)

-- Booleans
data False
data True

-- Conjunction
type family And a b
type instance And False b = False
type instance And True b  = b

-- Negation
type family Not a
type instance Not False = True
type instance Not True = False

-- Disjunction
type Or a b = (Not (And (Not a) (Not b)))

-- Implication
type Impl a b = (Not (And a (Not b)))

-- Equivalence
type Equiv a b = (And (Impl a b) (Impl b a))

-- Exclusive OR
type Xor a b = (Not (Equiv a b))

-- Conditional expression
type family If p t e
type instance If True t e  = t
type instance If False t e = e

-- Ordering
data EQ -- a = b
data LT -- a < b
data GT -- a > b

-- Ordering equality
type family OEqual a b
type instance OEqual EQ EQ = True
type instance OEqual EQ LT = False
type instance OEqual EQ GT = False
type instance OEqual LT EQ = False
type instance OEqual LT LT = True
type instance OEqual LT GT = False
type instance OEqual GT EQ = False
type instance OEqual GT LT = False
type instance OEqual GT GT = True

-- Number comparison
type family Compare a b
type instance Compare Z Z         = EQ
type instance Compare Z (S b)     = LT
type instance Compare Z (N b)     = GT
type instance Compare (S a) Z     = GT
type instance Compare (S a) (S b) = Compare a b
type instance Compare (S a) (N b) = GT
type instance Compare (N a) Z     = LT
type instance Compare (N a) (S b) = LT
type instance Compare (N a) (N b) = Compare a b

-- Equality and other number comparisons
type Equal a b = OEqual EQ (Compare a b)
type Less a b = OEqual LT (Compare a b)
type Greater a b = Less b a
type LessE a b = Or (Less a b) (Equal a b)
type GreaterE a b = LessE b a

-- At last, division
type family Div a b
type instance Div Z (S b)     = Z
type instance Div Z (N b)     = Z
type instance Div (S a) (S b) = If (Less a b)
                                   Z
                                   (Succ (Div (Sub a b) (S b)))
type instance Div (S a) (N b) = Neg (Div (S a) (S b))
type instance Div (N a) (S b) = Neg (Div (S a) (S b))
type instance Div (N a) (N b) = Div (S a) (S b)

-- Modulo
type family Mod a b
type instance Mod Z (S b)     = Z
type instance Mod Z (N b)     = Z
type instance Mod (S a) (S b) = If (Less a b)
                                   (S a)
                                   (Mod (Sub a b) (S b))
type instance Mod (N a) (S b) = Sub (S b) (Mod (S a) (S b))
type instance Mod (N a) (N b) = Neg (Mod (S a) (S b))