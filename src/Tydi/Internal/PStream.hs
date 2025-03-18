
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-} -- you need this to use C1-8 as a type argument

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

data PStreamTransfer c last stai strb n d u e = PStreamTransfer{
  dat   :: Vec n e,
  user  :: u,
  endi  :: Index n,
  strb  :: strb,
  last  :: last,
  stai  :: stai
}

type PStream c n d u e sealed = (ApplyComplexity c n d) n d u e sealed
data PStreamReady c n d u e = Ready | NotReady deriving (Show)

data Complexity = C Nat | CInherit -- why even add this wrapper? wouldn't Nat be fine?


type family ApplyComplexity (f :: Complexity) (n :: Nat) (d :: Nat) where
  ApplyComplexity (C 1) (n::Nat) (d::Nat) = PStreamX (C 1) (Vec d Bool)         ()        Bool
  ApplyComplexity (C 2) (n::Nat) (d::Nat) = PStreamX (C 2) (Vec d Bool)         ()        Bool
  ApplyComplexity (C 3) (n::Nat) (d::Nat) = PStreamX (C 3) (Vec d Bool)         ()        Bool
  ApplyComplexity (C 4) (n::Nat) (d::Nat) = PStreamX (C 4) (Vec d Bool)         ()        Bool
  ApplyComplexity (C 5) (n::Nat) (d::Nat) = PStreamX (C 5) (Vec n (Vec d Bool)) ()        Bool
  ApplyComplexity (C 6) (n::Nat) (d::Nat) = PStreamX (C 6) (Vec n (Vec d Bool)) (Index n) Bool
  ApplyComplexity (C 7) (n::Nat) (d::Nat) = PStreamX (C 7) (Vec n (Vec d Bool)) (Index n) (Vec n Bool)
  ApplyComplexity (C 8) (n::Nat) (d::Nat) = PStreamX (C 8) (Vec n (Vec d Bool)) (Index n) (Vec n Bool) --TODO check these!


-- some type families for extracting type parameters
type family Lanes p where
  Lanes (PStreamX c last stai strb n d u e sealed) = n
  Lanes (PStreamTransfer c last stai strb n d u e) = n

type family Dims p where
  Dims (PStreamX c last stai strb n d u e sealed) = d
  Dims (PStreamTransfer c last stai strb n d u e) = d

type family DataType p where
  DataType (PStreamX c last stai strb n d u e sealed) = e
  DataType (PStreamTransfer c last stai strb n d u e) = e

type family StaiType p where
  StaiType (PStreamX c last stai strb n d u e sealed) = stai
  StaiType (PStreamTransfer c last stai strb n d u e) = stai

type family StrbType p where
  StrbType (PStreamX c l s strb n d u e sealed) = strb
  StrbType (PStreamTransfer c l s strb n d u e) = strb

type family LastType p where
  LastType (PStreamX c last stai strb n d u e sealed) = last
  LastType (PStreamTransfer c last stai strb n d u e) = last
