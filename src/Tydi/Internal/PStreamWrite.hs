{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Tydi.Internal.PStreamWrite where

import Tydi.Internal.PStream
import Tydi.Internal.PStreamRead

import Clash.Explicit.Prelude hiding (last)
-- reading interfaces for physical streams, typed for various complexity levels
-- TODO



fromTransfer :: PStreamTransfer c last stai strb n d u e -> PStreamX c last stai strb n d u e 'False
fromTransfer PStreamTransfer{dat,user,stai,endi,last,strb} = PStream{valid=True,dat,user,stai,endi,last,strb}

fromTransferM :: NoTransfer (PStreamX c last stai strb n d u e 'False) => Maybe (PStreamTransfer c last stai strb n d u e) -> PStreamX c last stai strb n d u e 'False
fromTransferM (Just t) = fromTransfer t
fromTransferM Nothing = noTransfer

class NoTransfer a where
  noTransfer :: a
instance (
    Strb (PStreamX c last stai strb n d u e 'False),
    Last (PStreamX c last stai strb n d u e 'False),
    KnownNat n, KnownNat d
  ) => NoTransfer (PStreamX c last stai strb n d u e 'False) where
  noTransfer = PStream{
    valid=False,
    dat=repeat undefined,
    user=undefined,
    stai=undefined,
    endi=undefined,
    last=mkLast @(PStreamX c last stai strb n d u e 'False) $ repeat undefined,
    strb=mkStrb @(PStreamX c last stai strb n d u e 'False) $ repeat undefined
  }
