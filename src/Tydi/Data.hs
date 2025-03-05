{-# LANGUAGE UndecidableInstances #-}

module Tydi.Data where

import Clash.Explicit.Prelude

-- labels for Group/Union
newtype Label l a = L a deriving Show
type L l a = Label l a

newtype Group a = Group a deriving (Show,Generic,BitPack)

data Union a = Union{tag::Index (UnionCount a), bits:: BitVector (UnionWidth a)} deriving (Show, Generic, BitPack)

infixr 5 :*:
data (:*:) l r = Field l r deriving (Show, Generic, BitPack)

infixr 6 :+:
data (:+:) l r = Left l | Right r deriving (Show)

type family UnionCount x :: Nat where
  UnionCount (L _ _) = 1
  UnionCount (a :+: b) = UnionCount a + UnionCount b

type family UnionWidth x :: Nat where
  UnionWidth (L _ v) = BitSize v
  UnionWidth (a :+: b) = Max (UnionWidth a) (UnionWidth b)



-- shockwaves
-- deriving show and split for group, union

-- ...
