{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tydi.Data where

import Clash.Explicit.Prelude

-- labels for Group/Union
newtype L l a = L a deriving Show
--type L l a = Label l a

infixl 7 >::
type (>::) l a = L l a

-- groups
newtype Group a = Group a deriving (Show,Generic,BitPack)

infixr 5 :*:
data (:*:) l r = (:*:) l r deriving (Show, Generic, BitPack)

-- unions
data Union a = Union{
    tag::Index (UnionCount a),
    bits:: BitVector (UnionWidth a)
  } deriving (Generic)
deriving instance (KnownNat (UnionCount a), KnownNat (UnionWidth a)) => Show (Union a)
deriving instance (KnownNat (UnionCount a), KnownNat (UnionWidth a), 1 <= UnionCount a) => BitPack (Union a)

infixr 6 :+:
data (:+:) l r = Left l | Right r deriving (Show)

type family UnionCount x :: Nat where
  UnionCount (L _ _) = 1
  UnionCount (a :+: b) = UnionCount a + UnionCount b

type family UnionWidth x :: Nat where
  UnionWidth (L _ v) = BitSize v
  UnionWidth (a :+: b) = Max (UnionWidth a) (UnionWidth b)


-- bits
type Bits = BitVector

-- null
type Null = ()

-- isomorphics
-- TODO

-- Optics
-- TODO


-- bundle
-- TODO


-- Shockwaves
-- deriving show and split for group, union
-- TODO
