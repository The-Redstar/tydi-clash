{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Tydi.Synthesis where

import Clash.Explicit.Prelude

import Tydi.Internal.PStream
import Tydi.LStream
import Tydi.Data


-- synthesis
-- TODO
type Synth x = Synth' (C 0) 1 0 x

type family Synth' (c::Complexity) (t::Nat) (dim::Nat) x where
  Synth' c t dim (L l a)   = L l (Synth' c t dim a)
  Synth' c t dim (Group a) = Synth' c t dim a
  Synth' c t dim (a :*: b) = Synth' c t dim a :*: Synth' c t dim b
  Synth' c t dim (Union a) = Synth' c t dim (Group (ToGroup a))

  Synth' c t dim (LStream dim' sync 'Reverse force c' t' user dat) = Reverse (Synth' c t dim (LStream dim' sync 'Forward force c' t' user dat))
  Synth' c t dim (LStream dim' sync 'Forward force c' t' user dat) = StreamNode
    (PStream (CompInherit c c') (t*t') (SyncDim sync dim dim') user (RemoveStreams dat) 'True)
    (Synth'  (CompInherit c c') (t*t') (SyncDim sync dim dim') dat) -- TODO

  Synth' _ _ _ _ = ()

type family ToGroup a where
  ToGroup (a :+: b) = a :*: b
  ToGroup a = a

type family Reverse x where
  Reverse (StreamNode p h) = StreamNode (Reverse p) (Reverse h)

  Reverse (PStreamX c _ _ _ n d u e _sealed) = PStreamReady c n d u e
  Reverse (PStreamReady c n d u e) = PStream c n d u e 'True

  Reverse (L l a)   = L l (Reverse a)
  Reverse (Group a) = Group (Reverse a)
  Reverse (a :*: b) = Reverse a :*: Reverse b

  Reverse () = ()

  -- TODO

-- type family ReverseDir x where
--   ReverseDir 'Forward = 'Reverse
--   ReverseDir 'Reverse = 'Forward

type family SyncDim sm dprev dcur where
  SyncDim Sync       prev cur = prev + cur
  SyncDim Flatten    prev cur = cur
  SyncDim Desync     prev cur = prev + cur
  SyncDim FlatDesync prev cur = cur

type family CompInherit (cp :: Complexity) (c :: Complexity) where
  CompInherit cp CInherit = cp
  CompInherit _ c = c

type family RemoveStreams x where
  RemoveStreams (LStream _ _ _ _ _ _ _ _) = ()
  RemoveStreams (L l a)   = L l (RemoveStreams a)
  RemoveStreams (Group a) = Group (RemoveStreams a)
  RemoveStreams (Union a) = Union (RemoveStreams a)
  RemoveStreams (a :*: b) = RemoveStreams a :*: RemoveStreams b
  RemoveStreams (a :+: b) = RemoveStreams a :+: RemoveStreams b
  RemoveStreams x = x

-- stream node
data StreamNode p h = StreamNode{pstream::p,hierarchy::h}

-- optics
-- TODO

-- bundle
-- TODO
