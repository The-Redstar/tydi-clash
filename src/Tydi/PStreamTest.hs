{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}

module Tydi.PStreamTest where

-- import Data.Type.Ord (type (>=?))
-- import Data.Type.Bool (If)

import Clash.Explicit.Prelude hiding (last)
-- import GHC.TypeLits.KnownNat
-- import Clash.Sized.Internal.BitVector (xToBV)


data PStreamX c last stai strb n d e u sealed = PStream{
  valid :: Bool,
  dat  :: Vec n e,
  user :: u,
  endi :: Index n,
  strb :: strb--,
  -- last :: last,
  -- stai :: stai
}

type PStream c n d e u sealed = (c n d) n d e u sealed

type C1 n d = PStreamX 1 (Vec d Bool)         ()        ()
type C8 n d = PStreamX 8 (Vec n (Vec d Bool)) (Index n) (Vec n Bool)

class Strb n strb where
  getStrb :: PStreamX c l s strb n d e u seald -> Vec n Bool
  mkStrb :: Vec n Bool -> strb

instance (KnownNat n) => Strb n () where
  getStrb _ = repeat True
  mkStrb  _ = ()
instance Strb n (Vec n Bool) where
  getStrb PStream{strb} = strb
  mkStrb s = s

class Seal p q where
  seal :: p -> q

instance (Strb n strb, KnownNat n) => Seal (PStreamX c last stai strb n d e u 'False) (PStreamX c last stai strb n d e u 'True) where
  seal PStream{valid=False} = PStream {
    valid = False,
    dat   = repeat undefined,
    user  = undefined,
    endi  = undefined,
    strb  = mkStrb @n $ repeat undefined--,
    -- last  = mk,
    -- stai :: stai
  }
  seal p@PStream{dat,user,endi,strb} = PStream{
    valid = True,
    dat   = zipWith (\v s -> if s then v else undefined) dat (getStrb @n @strb p),
    user  = user,
    endi  = endi,
    strb  = strb
  }
