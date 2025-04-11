{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Tydi.Range (Range, pattern Range,range,safeRange,unsafeRange,start,end,contains,full) where
import Clash.Prelude

-- Inclusive index range meant for slicing vectors
data Range n = Range'{start::Index n,end::Index n} deriving (Show,Generic)
deriving instance (KnownNat n, 1<=n) => BitPack (Range n)
deriving instance (KnownNat n) => Lift (Range n)

pattern Range :: Index n -> Index n -> Range n
pattern Range start end <- Range'{start,end}  where
  Range start end = range start end
-- pattern Range :: Index n -> Index n -> Range n
-- pattern Range{startr,endr} <- Range'{start=startr,end=endr}
{-# COMPLETE Range #-}

range :: Index n -> Index n -> Range n
range a b = if a<=b then Range' a b else errorX "Range must be non-empty"
safeRange :: Index n -> Index n -> Range n
safeRange a b = Range' a (max a b)
unsafeRange :: Index n -> Index n -> Range n
unsafeRange = Range'

contains :: Range n -> Index n -> Bool
contains Range'{start,end} i = start<=i && i<=end

full :: (KnownNat n) => Range n
full = Range' 0 maxBound


-- autoReg
-- TODO
