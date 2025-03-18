


module Tydi.Slice (Slice, slice,start,end,strobed) where
import Clash.Explicit.Prelude hiding (slice)

-- closed interval, non-empty slice of a vector

data Slice n a = Slice{start::Index n,end::Index n,_vec::Vec n a}
slice :: (KnownNat n) => Index n -> Index n -> Vec n a -> Slice n a
slice s e v = Slice s e (zipWith (\i x -> if s<=i && i<=e then x else undefined) indicesI v)

strobed :: KnownNat n => Slice n a -> Vec n (Maybe a)
strobed (Slice s e v) = zipWith (\i x -> if s<=i && i<=e then Just x else Nothing) indicesI v

-- full -> slice 0 (n-1)
-- align -> aligns slice so start is at 0
-- sliceLength -> (e-s+1)

-- define !!, head, last, map, zipWithX, zipX, unzipX,foldr,foldl,foldr1,foldl1,fold, replace,reverse,length
-- bundle, unbundle
