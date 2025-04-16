{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Tydi.Connect where
import Clash.Explicit.Prelude
import Tydi.PStream hiding (PStream(..))
import Tydi.PStream (PStream)
import Tydi.Convert (TydiConvertible(..))

-- utilities for connecting pstreams and logical streams
-- TODO

class Connect p q where
  connect :: p -> q

instance (
    CompleteComplexity' (C c)  n d u  e
  , CompleteComplexity' (C c') n d u' e'
  , TydiConvertible e e'
  , TydiConvertible u u'
  , c<=c'
  ) => Connect (PStream (C c) n d u e) (PStream (C c') n d u' e') where
  connect = tfmap connect
instance (
    CompleteComplexity' (C c') n d u' e'
  , TydiConvertible e e'
  , TydiConvertible u u'
  , c<=c'
  ) => Connect (PStreamTransfer (C c) n d u e) (PStreamTransfer (C c') n d u' e') where
  connect p@PSTransfer{dat,user,endi} = PSTransfer{
    dat =convert <$> dat,
    user=convert user,
    last=mkLast  @(PStreamTransfer (C c') n d u' e') @(HasMultiLast (C c')) $ getLastExt p,
    strb=mkStrb' @(PStreamTransfer (C c') n d u' e') @(HasMultiStrb (C c')) $ getStrbExtRaw p,
    stai=mkStai  @(PStreamTransfer (C c') n d u' e') @(HasStai (C c')) $ getStaiExt p,
    endi=endi
   }

instance (TydiConvertible e' e, TydiConvertible u' u, c'<=c) => Connect (PStreamReady c n d u e) (PStreamReady c' n d u' e') where
  connect NotReady = NotReady
  connect Ready = Ready


-- TODO add connect for entire bundles
