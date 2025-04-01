{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances  #-}

module Tydi.Internal.PStreamRead where

import Clash.Explicit.Prelude hiding (last)
import qualified Clash.Prelude
import Tydi.Internal.PStream
import qualified Tydi.Slice as Slice

-- writing interfaces for physical streams for various complexity levels
-- TODO

{-
Note:
Raw: returns values unaffected by strb/endi/stai; otherwise, values are extended by default
Ext: extended; extends values to variants for complexity 8

-}


isValid :: PStreamX c last stai strb n d u e sealed -> Bool
isValid PStream{valid} = valid

unsafeGetTransfer :: PStreamX c last stai strb n d u e sealed -> PStreamTransfer c last stai strb n d u e
unsafeGetTransfer PStream{dat,user,endi,strb,last,stai} = PStreamTransfer{dat,user,endi,strb,last,stai}

getTransfer :: PStreamX c last stai strb n d u e sealed -> Maybe (PStreamTransfer c last stai strb n d u e)
getTransfer PStream{valid=False} = Nothing
getTransfer p = Just $ unsafeGetTransfer p


-- DATA
getDataRaw :: PStreamTransfer c last stai strb n d u e -> Vec n e
getDataRaw PStreamTransfer{dat} = dat

class Data p where
  type SliceType p
  getDataStrobed :: p -> Vec (Lanes p) (Maybe (DataType p))
  getDataSliced :: p -> SliceType p
instance ( Strb (PStreamTransfer c last stai Bool n d u e)
        , Stai (PStreamTransfer c last stai Bool n d u e)
        , KnownNat n
        ) => Data (PStreamTransfer c last stai Bool n d u e) where
  type SliceType (PStreamTransfer c last stai Bool n d u e) = Maybe (Slice.Slice n e)
  getDataStrobed p = zipWith fromBool (getStrbExt p) (getDataRaw p)
  getDataSliced p@PStreamTransfer{endi,dat} = fromBool (getStrb p) (Slice.slice (getStaiExt p) endi dat)
instance ( Strb (PStreamTransfer c last stai (Vec n Bool) n d u e)
         , Stai (PStreamTransfer c last stai (Vec n Bool) n d u e)
         , KnownNat n
         ) => Data (PStreamTransfer c last stai (Vec n Bool) n d u e) where
  type SliceType (PStreamTransfer c last stai (Vec n Bool) n d u e) = (Slice.Slice n (Maybe e))
  getDataStrobed p = zipWith fromBool (getStrbExt p) (getDataRaw p)
  getDataSliced p@PStreamTransfer{endi,dat} = Slice.slice (getStaiExt p) endi (zipWith fromBool (getStrbExt p) dat)


fromBool :: Bool -> a -> Maybe a
fromBool False _ = Nothing
fromBool True  x = Just x

-- USER
getUser :: PStreamTransfer c last stai strb n d u e -> u
getUser PStreamTransfer{user} = user

-- LAST
getLast :: PStreamTransfer c last stai strb n d u e -> last
getLast PStreamTransfer{last} = last
class Last p where
  getLastExt :: p -> Vec (Lanes p) (Vec (Dims p) Bool)
  mkLast :: Vec (Lanes p) (Vec (Dims p) Bool) -> LastType p
instance (KnownNat n, KnownNat d, n~n0+1) => Last (PStreamTransfer c (Vec d Bool) stai strb n d u e) where
  getLastExt p = reverse (getLast p :> repeat (repeat False))
  mkLast = Clash.Prelude.last
instance Last (PStreamTransfer c (Vec n (Vec d Bool)) stai strb n d u e) where
  getLastExt = getLast
  mkLast = id

-- STAI
class Stai p where
  getStaiExt :: p -> Index (Lanes p)
  mkStai :: Index (Lanes p) -> StaiType p
instance (KnownNat n) => Stai (PStreamTransfer c last () strb n d u e) where
  getStaiExt _ = 0
  mkStai _ = ()
instance (KnownNat n) => Stai (PStreamTransfer c last (Index n) strb n d u e) where
  getStaiExt PStreamTransfer{stai} = stai
  mkStai = id

-- ENDI
getEndi :: PStreamTransfer c last stai strb n d u e -> Index n
getEndi PStreamTransfer{endi} = endi

-- STAI+ENDI SLICE
sliceStrb :: (Stai (PStreamTransfer c last stai strb n d u e), KnownNat n) => PStreamTransfer c last stai strb n d u e -> Vec n Bool
sliceStrb p = map (\i -> getStaiExt p <=i && i<=getEndi p) indicesI

-- STRB
getStrbRaw :: PStreamTransfer c last stai strb n d u e -> strb
getStrbRaw PStreamTransfer{strb} = strb

class Strb p where
  getStrb :: p -> StrbType p
  getStrbExt :: p -> Vec (Lanes p) Bool
  getStrbExtRaw :: p -> Vec (Lanes p) Bool
  mkStrb :: StrbType p -> Vec (Lanes p) Bool -> StrbType p -- if single bit, keep 1st argument, else, take second argument
instance (Stai (PStreamTransfer c last stai Bool n d u e),KnownNat n) => Strb (PStreamTransfer c last stai Bool n d u e) where
  getStrb = getStrbRaw
  getStrbExt p = zipWith (&&) (sliceStrb p) (getStrbExtRaw p)
  getStrbExtRaw = repeat . getStrbRaw
  mkStrb singleStrb _slicedStrb = singleStrb
instance (Stai (PStreamTransfer c last stai (Vec n Bool) n d u e),KnownNat n) => Strb (PStreamTransfer c last stai (Vec n Bool) n d u e) where
  getStrb = getStrbExt
  getStrbExt p = zipWith (&&) (sliceStrb p) (getStrbExtRaw p)
  getStrbExtRaw = getStrbRaw
  mkStrb _oldStrb slicedStrb = slicedStrb

