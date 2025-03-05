
module Tydi.Synthesis where

import Clash.Explicit.Prelude

import Tydi.Internal.PStream
import Tydi.LStream
import Tydi.Data


-- synthesis
-- TODO
type Synth = Synth' 1 0

type family Synth' (t::Float) (dim::Nat) x where
  Synth' t dim (L l a)   = L l (Synth' t dim a)
  Synth' t dim (Group a) = Synth' t dim a
  Synth' t dim (a :*: b) = Synth' t dim a :*: Synth' t dim b
  Synth' t dim (Union a) = Synth' t dim (Group (ToGroup a))

  Synth' t dim (LStream dim' sync 'Reverse force c t' user dat) = Reverse (Synth' dim t (LStream dim' sync 'Forward force c t' user dat))
  Synth' t dim (LStream dim' sync 'Forward force c t' user dat) = StreamNode
    PStream c (t*t') (SyncDim dim dim') user (RemoveStreams dat)
    (Synth' (t*t') (SyncDim dim dim') dat) -- TODO

  Synth' _ _ _ = ()

type family ToGroup a where
  ToGroup (a :+: b) = a :*: b
  ToGroup a = a

type family Reverse x where
  Reverse 'Forward = 'Reverse
  Reverse 'Reverse = 'Forward

--  Reverse (PStream c n d u e _sealed) = PStreamReady c n d u e
--  Reverse (PStreamReady c n d u e) = PStream c n d u e 'True

type family SyncDim sm dprev dcur where
  SyncDim Sync       prev cur = prev + cur
  SyncDim Flatten    prev cur = cur
  SyncDim Desync     prev cur = prev + cur
  SyncDim FlatDesync prev cur = cur


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
