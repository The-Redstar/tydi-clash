{-# LANGUAGE UndecidableInstances #-}


module Tydi.Range (range,safeRange,unsafeRange,start,end,contains) where
import Clash.Prelude

-- Inclusive index range meant for slicing vectors
data Range n = Range{start::Index n,end::Index n} deriving (Show,Generic)
deriving instance (KnownNat n, 1<=n) => BitPack (Range n)

range :: Index n -> Index n -> Range n
range a b = if a<=b then Range a b else errorX "Range must be non-empty"
safeRange :: Index n -> Index n -> Range n
safeRange a b = Range a (max a b)
unsafeRange :: Index n -> Index n -> Range n
unsafeRange = Range

contains :: Index n -> Range n -> Bool
contains i Range{start,end} = start<=i && i<=end
