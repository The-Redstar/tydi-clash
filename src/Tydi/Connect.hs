{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Tydi.Connect where
import Clash.Explicit.Prelude
import Tydi.PStream hiding (PStream(..))
import Tydi.PStream (PStream)
import Tydi.Convert (TydiConvertible(..))
import Tydi.Data.Group
import Tydi.Synthesis

-- utilities for connecting pstreams and logical streams

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

instance (TydiConvertible e' e, TydiConvertible u' u, c'<=c) => Connect (PStreamReady (C c) n d u e) (PStreamReady (C c') n d u' e') where
  connect NotReady = NotReady
  connect Ready = Ready



-- connect entire bundles
instance (Connect p q, Connect s t) => Connect (StreamNode p s) (StreamNode q t) where
 connect StreamNode{stream,child} = StreamNode{stream=connect stream,child=connect child}

instance (Connect a b) => Connect (Group a) (Group b) where
  connect (Group x) = Group (connect x)
instance (Connect a b, Connect c d) => Connect (a :&: c) (b :&: d) where
  connect (x :&: y) = connect x :&: connect y
instance (Connect a b) => Connect (lbl' >:: a) (lbl' >:: b) where
  connect (L x) = L (connect x)

instance Connect () () where
  connect x = x
