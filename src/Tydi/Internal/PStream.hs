
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Tydi.Internal.PStream where

import Clash.Explicit.Prelude hiding (last)


data PStreamX c last stai strb n d u e sealed = PStream{
  valid :: Bool,
  dat   :: Vec n e,
  user  :: u,
  endi  :: Index n,
  strb  :: strb,
  last  :: last,
  stai  :: stai
} deriving (Generic, ShowX)


type PStream c n d u e sealed = (c n d) n d u e sealed
data PStreamReady c n d u e = Ready | NotReady deriving (Show)

-- complexity levels   C last                 stai      strb
type C1 n d = PStreamX 1 (Vec d Bool)         ()        ()
type C2 n d = PStreamX 2 (Vec d Bool)         ()        ()
type C3 n d = PStreamX 3 (Vec d Bool)         ()        ()
type C4 n d = PStreamX 4 (Vec d Bool)         ()        ()
type C5 n d = PStreamX 5 (Vec n (Vec d Bool)) ()        ()
type C6 n d = PStreamX 6 (Vec n (Vec d Bool)) (Index n) ()
type C7 n d = PStreamX 7 (Vec n (Vec d Bool)) (Index n) (Vec n Bool)
type C8 n d = PStreamX 8 (Vec n (Vec d Bool)) (Index n) (Vec n Bool) --TODO check these!

class Stai n stai where
  getStai :: PStreamX c l stai s n d u e sealed -> Index n
  mkStai :: Index n -> stai
instance (KnownNat n) => Stai n () where
  getStai _ = 0
  mkStai  _ = ()
instance (KnownNat n) => Stai n (Index n) where
  getStai PStream{stai} = stai
  mkStai  i = i

class Strb n strb where
  getStrb :: PStreamX c l s strb n d u e sealed -> Vec n Bool
  mkStrb  :: Vec n Bool -> strb
instance (KnownNat n) => Strb n () where
  getStrb _ = repeat True
  mkStrb  _ = ()
instance Strb n (Vec n Bool) where
  getStrb PStream{strb} = strb
  mkStrb s = s



-- sealing streams
class Seal p q where
  seal :: p -> q

instance (Strb n strb, Stai n stai, KnownNat n) => Seal (PStreamX c last stai strb n d u e sealed) (PStreamX c last stai strb n d u e 'True) where
  seal PStream{valid=False} = PStream {
    valid = False,
    dat   = repeat undefined,
    user  = undefined,
    stai  = undefined,
    endi  = undefined,
    strb  = mkStrb @n $ repeat undefined,
    last  = undefined
  }
  seal p@PStream{dat,user,stai,endi,last} = PStream{
    valid = True,
    dat   = dat',
    user  = user,
    stai  = stai,
    endi  = endi,
    strb  = mkStrb @n @strb strb',
    last  = last
  }
    where
      dat'  = zipWith3 god indicesI dat (getStrb @n @strb p) -- constrain by stai/endi, then strb
      strb' = zipWith  gos indicesI     (getStrb @n @strb p) -- constrain by stai/endi
      god i d s = if (i >= getStai @n @stai p) && (i <= endi) && s then d else undefined
      gos i s   = if (i >= getStai @n @stai p) && (i <= endi)      then s else undefined




-- Show
instance (ShowX strb, ShowX last, ShowX stai, ShowX e, ShowX u) => Show (PStreamX c last stai strb n d u e sealed) where
  show = showX


-- Shockwaves
-- TODO
-- Shows internal signals, and colors them based on strobe etc.
