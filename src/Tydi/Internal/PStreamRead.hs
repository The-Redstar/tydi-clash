{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances  #-}

module Tydi.Internal.PStreamRead where

import Clash.Explicit.Prelude hiding (last)
import Tydi.Internal.PStream

-- writing interfaces for physical streams for various complexity levels
-- TODO


isValid :: PStreamX c last stai strb n d u e sealed -> Bool
isValid PStream{valid} = valid

getTransfer :: PStreamX c last stai strb n d u e sealed -> Maybe (PStreamTransfer c last stai strb n d u e)
getTransfer PStream{valid=False} = Nothing
getTransfer PStream{dat,user,endi,strb,last,stai} = Just PStreamTransfer{dat,user,endi,strb,last,stai}


{-

PStream:
  - getStai -> Maybe stai
  - unsafeGetStai -> stai
  - mkStai -> stai

PStreamTransfer
  - getStai -> stai
  - mkStai -> stai

-}



type family SafeStaiType p where
  SafeStaiType (PStreamX c last stai strb n d u e sealed) = Maybe (Index n)
  SafeStaiType (PStreamTransfer c last stai strb n d u e) = Index n




type family SafeStrbType p where
  SafeStrbType (PStreamX c last stai strb n d u e sealed) = Maybe (Vec n Bool)
  SafeStrbType (PStreamTransfer c last stai strb n d u e) = Vec n Bool


-- STAI
class Stai p where
  getStai :: p -> SafeStaiType p -- ^ Get the start index if there is a valid transfer. If there is no start index, defaults to 0.
  unsafeGetStai :: p -> Index (Lanes p) -- ^ Get the start index if it exists; otherwise, return 0.
  unsafeGetStai _ = undefined
  mkStai :: Index (Lanes p) -> StaiType p
instance (KnownNat n) => Stai (PStreamX c last () strb n d u e sealed)where
  getStai PStream{valid=True} = Just 0
  getStai PStream{} = Nothing
  unsafeGetStai PStream{} = 0
  mkStai  _ = ()
instance (KnownNat n) => Stai (PStreamTransfer c last () strb n d u e) where
  getStai _ = 0
  mkStai  _ = ()
instance (KnownNat n) => Stai (PStreamX c l (Index n) strb n d u e sealed) where
  getStai PStream{valid=True,stai} = Just stai
  getStai PStream{} = Nothing
  unsafeGetStai PStream{stai} = stai
  mkStai  i = i
instance (KnownNat n) => Stai (PStreamTransfer c last (Index n) strb n d u e) where
  getStai PStreamTransfer{stai} = stai
  mkStai  i = i

-- STRB
class Strb p where
  getStrb :: p -> SafeStrbType p
  unsafeGetStrb :: p -> Vec (Lanes p) Bool
  unsafeGetStrb _ = undefined
  mkStrb  :: Vec (Lanes p) Bool -> StrbType p
instance (KnownNat n) => Strb (PStreamX c l s () n d u e sealed) where
  getStrb PStream{valid=True} = Just $ repeat True
  getStrb _ = Nothing
  unsafeGetStrb _ = repeat True
  mkStrb  _ = ()
instance Strb (PStreamX c l s (Vec n Bool) n d u e sealed) where
  getStrb PStream{valid=True,strb} = Just strb
  getStrb _ = Nothing
  unsafeGetStrb PStream{strb} = strb
  mkStrb s = s
instance (KnownNat n) => Strb (PStreamTransfer c l s () n d u e) where
  getStrb _ = repeat True
  mkStrb  _ = ()
instance Strb (PStreamTransfer c l s (Vec n Bool) n d u e) where
  getStrb PStreamTransfer{strb} = strb
  mkStrb s = s

-- LAST
class Last p where
  mkLast  :: Vec (Lanes p) (Vec (Dims p) Bool) -> LastType p
instance (KnownNat n,KnownNat d) => Last (PStreamX c (Vec n (Vec d Bool)) s st n d u e sealed) where
  mkLast l = l
instance (KnownNat d, n~n1+1) => Last (PStreamX c (Vec d Bool) s st n d u e sealed) where
  mkLast = head
instance (KnownNat n,KnownNat d) => Last (PStreamTransfer c (Vec n (Vec d Bool)) s st n d u e) where
  mkLast l = l
instance (KnownNat d, n~n1+1) => Last (PStreamTransfer c (Vec d Bool) s st n d u e) where
  mkLast = head

-- DATA, USER, ENDI, LAST
class GetPStreamFields a where
  type SafeDataType a
  type SafeUserType a
  type SafeEndiType a
  type SafeLastType a
  rawGetData :: a -> SafeDataType a -- ^ Get the data lanes if there is a transfer. Note that these fields might not all contain valid data!
  getUser :: a -> SafeUserType a -- ^ Get the `user` data if there is a transfer.
  getEndi :: a -> SafeEndiType a -- ^ Get the `endi` index if there is a transfer.
  getLast :: a -> SafeLastType a -- ^ Get the `last` data if there is a transfer.

instance GetPStreamFields (PStreamX c last stai strb n d u e sealed) where
  type SafeDataType (PStreamX c last stai strb n d u e sealed) = Maybe (Vec n e)
  type SafeUserType (PStreamX c last stai strb n d u e sealed) = Maybe u
  type SafeEndiType (PStreamX c last stai strb n d u e sealed) = Maybe (Index n)
  type SafeLastType (PStreamX c last stai strb n d u e sealed) = Maybe last
  rawGetData PStream{valid=False} = Nothing
  rawGetData PStream{dat} = Just dat
  getUser PStream{valid=False} = Nothing
  getUser PStream{user} = Just user
  getEndi PStream{valid=False} = Nothing
  getEndi PStream{endi} = Just endi
  getLast PStream{valid=False} = Nothing
  getLast PStream{last} = Just last

instance GetPStreamFields (PStreamTransfer c last stai strb n d u e) where
  type SafeDataType (PStreamTransfer c last stai strb n d u e) = Vec n e
  type SafeUserType (PStreamTransfer c last stai strb n d u e) = u
  type SafeEndiType (PStreamTransfer c last stai strb n d u e) = Index n
  type SafeLastType (PStreamTransfer c last stai strb n d u e) = last
  rawGetData PStreamTransfer{dat} = dat
  getUser PStreamTransfer{user} = user
  getEndi PStreamTransfer{endi} = endi
  getLast PStreamTransfer{last} = last

-- DATA (safer)
class GetPStreamData a where
  type DataStrobedType a
  getDataStrobed :: a -> DataStrobedType a -- ^ Get a vector of maybe values for all data lanes. Values take into account `endi`, `stai` (if it exists), and `strb` (if it exists).

instance (
    DataStrobedType (PStreamTransfer c last stai strb n d u e) ~ Vec n (Maybe e),
    Stai (PStreamTransfer c last stai strb n d u e),
    Strb (PStreamTransfer c last stai strb n d u e),
    KnownNat n
  ) => GetPStreamData (PStreamX c last stai strb n d u e sealed) where
  type DataStrobedType (PStreamX c last stai strb n d u e sealed) = Maybe (Vec n (Maybe e))
  getDataStrobed p = getDataStrobed <$> getTransfer p

instance (
    Stai (PStreamTransfer c last stai strb n d u e),
    Strb (PStreamTransfer c last stai strb n d u e),
    KnownNat n
  ) => GetPStreamData (PStreamTransfer c last stai strb n d u e) where
  type DataStrobedType (PStreamTransfer c last stai strb n d u e) = Vec n (Maybe e)
  getDataStrobed p@PStreamTransfer{dat,endi} = zipWith3 god indicesI dat (getStrb p)
    where
      god i d s = if (i >= getStai p) && (i <= endi) && s then Just d else Nothing


