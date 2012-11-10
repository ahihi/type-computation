{-# LANGUAGE TypeFamilies #-}

module Ordering
    ( LT
    , EQ
    , GT
    , OEq
    ) where

import Boolean
    ( False
    , True
    )

-- Ordering
data LT -- a < b
data EQ -- a = b
data GT -- a > b

-- Ordering equality
type family OEq a b
type instance OEq EQ EQ = True
type instance OEq EQ LT = False
type instance OEq EQ GT = False
type instance OEq LT EQ = False
type instance OEq LT LT = True
type instance OEq LT GT = False
type instance OEq GT EQ = False
type instance OEq GT LT = False
type instance OEq GT GT = True
