{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Tydi.Synthesis where

import Clash.Explicit.Prelude

import Optics.Lens

import Tydi.PStream
import Tydi.LStream
import Tydi.Data
import Optics.Core
import Data.Typeable

import Shockwaves


-- synthesis
type Synth x = Synth' (C 0) 1 0 x

type family Synth' (c::Complexity) (t::Nat) (dim::Nat) x where
  Synth' c t dim (l >:: a) = l >:: Synth' c t dim a
  Synth' c t dim (Group a) = Group (Synth' c t dim a)
  Synth' c t dim (a :&: b) = Synth' c t dim a :&: Synth' c t dim b
  Synth' c t dim (Union a) = Synth' c t dim (Group (ToGroup a))

  Synth' c t dim (LStream dim' sync 'Reverse force c' t' user dat) = Reverse (Synth' c t dim (LStream dim' sync 'Forward force c' t' user dat))
  Synth' c t dim (LStream dim' sync 'Forward force c' t' user dat) = StreamNode
    (PStream (CompInherit c c') (t*t') (SyncDim sync dim dim') user (RemoveStreams dat))
    (Synth'  (CompInherit c c') (t*t') (SyncDim sync dim dim') dat) -- TODO: other throughput type

  Synth' _ _ _ _ = ()

type family ToGroup a where
  ToGroup (a :|: b) = a :&: b
  ToGroup a = a

type family Reverse x where
  Reverse (StreamNode p h) = StreamNode (Reverse p) (Reverse h)

  Reverse (PStream c n d u e ) = PStreamReady c n d u e
  Reverse (PStreamReady c n d u e) = PStream c n d u e

  Reverse (l >:: a) = l >:: Reverse a
  Reverse (a :&: b) = Reverse a :&: Reverse b
  Reverse (Group a) = Group (Reverse a)

  Reverse () = ()

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
  RemoveStreams (l >:: a)   = l >:: RemoveStreams a
  RemoveStreams (Group a) = Group (RemoveStreams a)
  RemoveStreams (Union a) = Union (RemoveStreams a)
  RemoveStreams (a :&: b) = RemoveStreams a :&: RemoveStreams b
  RemoveStreams (a :|: b) = RemoveStreams a :|: RemoveStreams b
  RemoveStreams x = x

-- stream node
data StreamNode p h = StreamNode{stream::p,child::h} deriving (Show,Generic,Display,Split,NFDataX,Typeable,BitPack)
type family ChildType a where
  ChildType (StreamNode _ c) = c
type family StreamType a where
  StreamType (StreamNode p _) = p


-- optics
_stream :: Lens (StreamNode p h) (StreamNode p' h) p p'
_stream = lens (\StreamNode{stream} -> stream) (\sn stream -> sn{stream})

_child :: Lens (StreamNode p h) (StreamNode p h') h h'
_child = lens (\StreamNode{child} -> child) (\sn child -> sn{child})

-- bundle/unbundle
instance (Bundle h) => Bundle (StreamNode p h) where
  type Unbundled dom (StreamNode p h) = (StreamNode (Signal dom p) (Unbundled dom h))
  bundle (StreamNode ps hs) = StreamNode <$> ps <*> bundle hs
  unbundle nodes = StreamNode ((^. getting _stream) <$> nodes) $ unbundle ((^. getting _child) <$> nodes)

-- TODO: flatten? make thigns more like the Tydi specification
