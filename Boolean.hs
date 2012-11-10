{-# LANGUAGE TypeFamilies #-}

module Boolean
    ( False
    , True
    , And
    , Not
    , Or
    , Impl
    , Equiv
    , Xor
    , If
    ) where

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
