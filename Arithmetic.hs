{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Arithmetic where

import Boolean
    ( False
    , True
    , And
    , If
    )
import Ordering
    ( LT
    , EQ
    , GT
    , OEq
    )

-- Natural numbers
data Z   -- Zero
data S n -- Successor of n (n = Z or n = S n')

type N0 = Z
type N1 = S N0
type N2 = S N1

-- Natural comparison
type family NCmp m n
type instance NCmp Z Z           = EQ
type instance NCmp Z (S n')      = LT
type instance NCmp (S m') Z      = GT
type instance NCmp (S m') (S n') = NCmp m' n'

-- Natural addition
type family NAdd m n
type instance NAdd m Z      = m
type instance NAdd m (S n') = NAdd (S m) n'

-- Natural subtraction
type family NSub m n
type instance NSub m Z           = m
type instance NSub Z Z           = Z
type instance NSub (S m') (S n') = NSub m' n'

-- Natural multiplication
type family NMul m n
type instance NMul m Z      = N0
type instance NMul m (S n') = NAdd m (NMul m n')

-- Natural division
type family NDiv m n
type instance NDiv Z (S n')      = Z
type instance NDiv (S m') (S n') = If (OEq LT (NCmp m' n'))
                                      Z
                                      (S (NDiv (NSub m' n') (S n')))

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
type instance IFromN n = I n N0

-- Integer to natural
type family NFromI n
type instance NFromI (I m Z) = m

-- Integer comparison
type family ICmp m n
type instance ICmp (I am bm) (I an bn) = NCmp (NAdd am bn) (NAdd an bm)

-- Integer normalization
type family INorm n
type instance INorm (I a Z)           = I a Z
type instance INorm (I Z b)           = I Z b
type instance INorm (I (S a') (S b')) = I a' b'

-- Integer signum
type family ISign n
type instance ISign n = If (OEq LT (ICmp n I0))
                           I_1
                           (If (OEq GT (ICmp n I0))
                               I1
                               I0)

-- Integer absolute value
type family IAbs n
type instance IAbs n = IMul n (ISign n)

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

-- Integer division
type family IDiv m n
type instance IDiv m n
    = IMul (IFromN (NDiv (NFromI (IAbs m)) (NFromI (IAbs n)))) (IMul (ISign m) (ISign n))

-- Integer power (with natural exponent)
type family IPow m n
type instance IPow m Z      = I1
type instance IPow m (S n') = IMul m (IPow m n')
