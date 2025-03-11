{-# LANGUAGE UndecidableInstances #-}

module PStreamSeal where


import Tydi.Internal.PStream
import Tydi.Internal.PStreamRead
import Clash.Explicit.Prelude hiding (last)

-- sealing streams
class Seal p where
  type SEALED p
  seal :: p -> SEALED p

instance (
    Strb (PStreamX c last stai strb n d u e sealed),
    Stai (PStreamX c last stai strb n d u e sealed),
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
    strb  = mkStrb @(PStreamX c last stai strb n d u e sealed) $ repeat undefined,
    last  = undefined
  }
  seal p@PStream{dat,user,stai,endi,last} = PStream{
    valid = True,
    dat   = dat',
    user  = user,
    stai  = stai,
    endi  = endi,
    strb  = mkStrb @(PStreamX c last stai strb n d u e sealed) strb',
    last  = last
  }
    where
      dat'  = zipWith3 god indicesI dat (unsafeGetStrb p) -- constrain by stai/endi, then strb
      strb' = zipWith  gos indicesI     (unsafeGetStrb p) -- constrain by stai/endi
      god i d s = if (i >= unsafeGetStai p) && (i <= endi) && s then d else undefined
      gos i s   = if (i >= unsafeGetStai p) && (i <= endi)      then s else undefined
