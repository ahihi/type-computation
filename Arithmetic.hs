{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Arithmetic where

-- Natural numbers
data Z   -- Zero
data S n -- Successor of n (n = Z or n = S n')

type N0 = Z
type N1 = S N0

-- Natural addition
type family NAdd m n
type instance NAdd m Z      = m
type instance NAdd m (S n') = NAdd (S m) n'

-- Natural multiplication
type family NMul m n
type instance NMul m Z      = N0
type instance NMul m (S n') = NAdd m (NMul m n')

-- Natural power
type family NPow m n
type instance NPow m Z      = N1
type instance NPow m (S n') = NMul m (NPow m n')

-- Integers
data I a b -- Integers of the form a-b (a and b are natural numbers)

type I0 = I N0 N0
type I1 = I N1 N0

-- Integer normalization
type family INorm n
type instance INorm (I a Z)           = I a Z
type instance INorm (I Z b)           = I Z b
type instance INorm (I (S a') (S b')) = I a' b'

-- Integer addition
type family IAdd m n
type instance IAdd (I am bm) (I an bn)
    = INorm (I (NAdd am an) (NAdd bm bn))

-- Integer negation
type family INeg n
type instance INeg (I a b) = INorm (I b a)

-- Integer subtraction
type family ISub m n
type instance ISub m n = IAdd m (INeg n)

-- Integer multiplication
type family IMul m n
type instance IMul (I am bm) (I an bn)
    = INorm (I (NAdd (NMul am an) (NMul bm bn)) (NAdd (NMul am bn) (NMul bm an)))

-- Integer power (with natural exponent)
type family IPow m n
type instance IPow m Z      = I1
type instance IPow m (S n') = IMul m (IPow m n')

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
type Or a b = Not (And (Not a) (Not b))

-- Implication
type Impl a b = Not (And a (Not b))

-- Equivalence
type Equiv a b = And (Impl a b) (Impl b a)

-- Exclusive OR
type Xor a b = Not (Equiv a b)

-- Conditional expression
type family If p t e
type instance If True t e  = t
type instance If False t e = e
