{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tydi.PStream where

import Data.Type.Ord (type (>=?))
import Data.Type.Bool (If)

import Clash.Explicit.Prelude hiding (last)
import GHC.TypeLits.KnownNat


-- Physical stream

data PStream c n d e u sealed = -- complexity, lanes, dimension, data type, user data
  PStream {
    valid :: Bool,
    data' :: Vec n e,
    user :: u,
    strb :: StrbType c n,
    last :: Vec n (Vec d Bool),
    stai :: StaiType c n,
    endi :: Index n }

data PStreamX meta n e u sealed =
  PStreamX {
    valid :: Bool,
    dat   :: Vec n e,
    user  :: u,
    meta  :: meta,
    endi  :: Index n
  }

newtype Level1 (n :: Nat) d = Level1 {last :: Vec d Bool}
  deriving (SafeStrb n)

newtype Level2 (n :: Nat) d = Level2 {last :: Vec d Bool}
  deriving (SafeStrb n)

newtype Level3 (n :: Nat) d = Level3 {last :: Vec d Bool}
  deriving (SafeStrb n)

newtype Level4 (n :: Nat) d = Level4 {last :: Vec d Bool}
  deriving (SafeStrb n)

newtype Level5 (n :: Nat) d = Level5 {last :: Vec d Bool}
  deriving (SafeStrb n)

data Level6 n d = Level6 {last :: Vec d Bool, stai :: Index n}
  deriving (SafeStrb n)

data Level7 n d = Level7 {last :: Vec d Bool, stai :: Index n, strb :: Vec n Bool}

instance KnownNat n => SafeStrb n (Level7 n d) where
  safeStrb (Level7 {strb=s}) = s

data Level8 n d = Level8 {last :: Vec n (Vec d Bool), stai :: Index n, strb :: Vec n Bool}

data PStreamReady c n d e u = Ready | NotReady deriving (Show,BitPack,Generic)

newtype IfN c a b = IfN (If c a b)

type LastType c n d = IfN (c>=?8) (Vec n (Vec d Bool)) (Vec d Bool)
type StrbType c n   = IfN (c>=?7) (Vec n Bool)         ()
type StaiType c n   = IfN (c>=?6) (Index n)            ()


-- sealing


class Sealable a b where
  seal :: a -> b
instance (KnownNat n,KnownNat d,KnownNat c) => Sealable (PStream c n d e u False) (PStream c n d e u True) where
  seal PStream{valid=False} = PStream{
    valid = False,
    data' = repeat undefined,
    user  = undefined,
    strb  = fillStrb undefined,
    last  = repeat $ repeat undefined,
    stai  = undefined,
    endi  = undefined
  }

  seal PStream{valid, data', user, strb, last, stai, endi} = PStream{
    valid = valid,
    data' = zipWith3 god indicesI data' (safeStrb @n strb), -- fill based on strb, stai, endi
    user  = user,
    strb  = getStrb $ zipWith gos indicesI $ safeStrb strb,
    last  = last, -- does this need any complexity-dependent checking?
    stai  = stai,
    endi  = endi
  }
    where god i d s = if (i >= safeStai @n stai) && (i <= endi) && s then d else undefined
          gos i s   = if (i >= safeStai @n stai) && (i <= endi)      then s else undefined

-- strobe
class FillStrb s where
  fillStrb :: Bool -> s
instance (KnownNat n) => FillStrb (Vec n Bool) where
  fillStrb = repeat
instance FillStrb () where
  fillStrb _ = ()

class (KnownNat n) => GetStrb n s where
  getStrb :: Vec n Bool -> s
instance (KnownNat n) => GetStrb n (Vec n Bool) where
  getStrb x = x
instance (KnownNat n) => GetStrb n () where
  getStrb _ = ()
instance (GetStrb n a, GetStrb n b, KnownBool c) => GetStrb n (IfN c a b) where
  getStrb v = case boolSing @c of
     STrue -> IfN (getStrb @n @a v)
     SFalse -> IfN (getStrb @n @b v)

class (KnownNat n) => SafeStrb n a where
  safeStrb :: a -> Vec n Bool
  safeStrb _ = repeat True
instance (KnownNat n) => SafeStrb n (Vec n Bool) where
  safeStrb s = s
instance (KnownNat n) => SafeStrb n () where
  safeStrb _ = repeat True
instance (SafeStrb n a, SafeStrb n b, KnownBool c) => SafeStrb n (IfN c a b) where
  safeStrb (IfN x) = case boolSing @c of
     STrue -> safeStrb @n @a x
     SFalse -> safeStrb @n @b x



-- start index
class (KnownNat n) => SafeStai n a where
  safeStai :: a -> Index n
instance (KnownNat n) => SafeStai n (Index n) where
  safeStai s = s
instance (KnownNat n) => SafeStai n () where
  safeStai _ = maxBound
-- instance (SafeStai n a, SafeStai n b, KnownBool c) => SafeStai n (IfN c a b) where
--   safeStai x = case boolSing @c of
--      STrue -> safeStai @n @a x
--      SFalse -> safeStai @n @b x



-- ShockWaves

-- ...
