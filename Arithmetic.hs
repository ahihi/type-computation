{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Arithmetic where

import Boolean
    ( False
    , True
    , And
    )

-- Natural numbers
data Z   -- Zero
data S n -- Successor of n (n = Z or n = S n')

type N0 = Z
type N1 = S N0
type N2 = S N1

-- Natural equality
type family NEq m n
type instance NEq Z Z           = True
type instance NEq Z (S n')      = False
type instance NEq (S m') Z      = False
type instance NEq (S m') (S n') = NEq m' n'

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

type I_1 = I N0 N1
type I0 = I N0 N0
type I1 = I N1 N0
type I2 = I N2 N0

-- Integer from natural
type family IFromN n
type instance IFromN n = I n I0

-- Integer equality
type family IEq m n
type instance IEq m n = INormEq (INorm m) (INorm n)

-- Normalized integer equality (helper function for IEq)
type family INormEq m n
type instance INormEq (I am bm) (I an bn) = And (NEq am an) (NEq bm bn)

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
