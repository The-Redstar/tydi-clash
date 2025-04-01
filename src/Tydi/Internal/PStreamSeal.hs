{-# LANGUAGE UndecidableInstances #-}

module Tydi.Internal.PStreamSeal (Seal(..)) where


import Tydi.Internal.PStream
import Tydi.Internal.PStreamRead
import Clash.Explicit.Prelude hiding (last)
import qualified Clash.Sized.Vector as Vector
import Data.Maybe (fromJust)
import Tydi.Internal.PStream (PStreamTransfer(PStreamTransfer))


-- sealing streams
class Seal p where
  type SEALED p
  seal :: p -> SEALED p

instance (
    Strb (PStreamTransfer c last stai strb n d u e),
    Stai (PStreamTransfer c last stai strb n d u e),
    Last (PStreamTransfer c last stai strb n d u e),
    Data (PStreamTransfer c last stai strb n d u e),
    KnownNat d,
    KnownNat n
  ) => Seal (PStreamX c last stai strb n d u e sealed)  where
  type SEALED (PStreamX c last stai strb n d u e sealed) = PStreamX c last stai strb n d u e 'True
  seal :: PStreamX c last stai strb n d u e sealed -> PStreamX c last stai strb n d u e 'True
  seal PStream{valid=False} = PStream {
    valid = False,
    dat   = repeat undefined,
    user  = undefined,
    stai  = undefined,
    endi  = undefined,
    strb  = mkStrb @(PStreamTransfer c last stai strb n d u e) undefined $ repeat undefined,
    last  = mkLast @(PStreamTransfer c last stai strb n d u e) $ repeat $ repeat undefined
  }
  seal p@PStream{dat,user,stai,endi,last,strb} = PStream{
    valid = True,
    dat   = dat', -- everything outside stai,endi,strb set to undefined
    user  = user,
    stai  = stai, -- stai if present
    endi  = endi,
    strb  = strb', -- if single bit strobe, keep strobe, else, set all outside stai,endi to undefined
    last  = last
  }
    where
      dat'  = map fromJust (unsafeGetDataStrobed p)
      strb' = mkStrb @(PStreamTransfer c last stai strb n d u e) strb strb'' -- select between original and undefined sliced
      strb'' = zipWith go indicesI $ unsafeGetStrbExt p -- replace values outside stai,endi with undefined
      go i s = if (i >= unsafeGetStaiExt p) && (i <= endi) then s else undefined


      gos i s   = if (i >= unsafeGetStaiExt p) && (i <= endi)      then s else undefined
-- TODO: Sealing transfers
-- TODO: remove sealed parameter from type


unsafeGetDataStrobed ::
  ( Stai (PStreamTransfer c last stai strb n d u e)
  , Strb (PStreamTransfer c last stai strb n d u e)
  , Data (PStreamTransfer c last stai strb n d u e)
  ) => PStreamX c last stai strb n d u e sealed -> Vec n (Maybe e)
unsafeGetDataStrobed = getDataStrobed . unsafeGetTransfer
unsafeGetStaiExt :: (Stai (PStreamTransfer c last stai strb n d u e)) => PStreamX c last stai strb n d u e sealed -> Index n
unsafeGetStaiExt = getStaiExt . unsafeGetTransfer
unsafeGetStrbExt :: (Strb (PStreamTransfer c last stai strb n d u e)) => PStreamX c last stai strb n d u e sealed -> Vec n Bool
unsafeGetStrbExt = getStrbExtRaw . unsafeGetTransfer
