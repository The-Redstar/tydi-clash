{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tydi.PStreamTest where

import Data.Type.Ord (type (>=?))
import Data.Type.Bool (If)

import Clash.Explicit.Prelude hiding (last)
import GHC.TypeLits.KnownNat
import Clash.Sized.Internal.BitVector (xToBV)


data PStream c n = PStream {
  valid::Bool,
  strb::StrbType c n
}

data C1 = C1
data C8 = C8

type family StrbType c n where
  StrbType C1 n = ()
  StrbType C8 n = Vec n Bool

class GetStrb n a where
  getStrb :: a -> Vec n Bool
instance (KnownNat n) => GetStrb n (Vec n Bool) where
  getStrb :: KnownNat n => Vec n Bool -> Vec n Bool
  getStrb x = x
instance (KnownNat n) => GetStrb n () where
  getStrb _ = repeat True

class Seal p where
  seal :: p -> p

class HasStrb c
class NoStrb c

class PGetStrb p where
  getS :: p -> Vec n Bool

instance (HasStrb c) => PGetStrb (PStream c n) where
  getS PStream{strb} = strb
instance (NoStrb  c) => PGetStrb (PStream c n) where
  getS _ = repeat True :: Vec n Bool

instance (KnownNat n) => Seal (PStream c n) where
  seal p@PStream{strb} = p
    where s = getS @c
