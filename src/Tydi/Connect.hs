{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}


module Tydi.Connect where
import Clash.Explicit.Prelude
import Tydi.PStream hiding (PStream(..))
import Tydi.PStream (PStream)
import Tydi.Convert (TydiConvertible(..))

-- utilities for connecting pstreams and logical streams
-- TODO

class Connect p q where
  connect :: p -> q

-- instance (
--     CompleteComplexity' c n d u e
--   , CompleteComplexity' c' n d u' e'
--   , TydiConvertible e e'
--   , TydiConvertible u u'
--   , c<=c'
--   ) => Connect (PStream c n d u e) (PStream c' n d u' e') where
--   connect = tfmap connect
-- instance (
--     CompleteComplexity' c' n d u' e'
--   , TydiConvertible e e'
--   , TydiConvertible u u'
--   , c<=c'
--   ) => Connect (PStreamTransfer c n d u e) (PStreamTransfer c' n d u' e') where
--   connect p@PSTransfer{dat,user,endi} = convert <$> PSTransfer{
--     dat =convert <$> dat,
--     user=convert user,
--     last=mkLast $ getLastExt p,
--     strb=mkStrb' $ getStrbExtRaw p,
--     stai=mkStai $ getStaiExt p,
--     endi=endi
--    }

instance (TydiConvertible e' e, TydiConvertible u' u, c'<=c) => Connect (PStreamReady c n d u e) (PStreamReady c' n d u' e') where
  connect NotReady = NotReady
  connect Ready = Ready


