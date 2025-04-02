

module Tydi.Prefix where
import Clash.Explicit.Prelude


-- like Slice, but without a start index
data Prefix n a = Prefix{end::Index n,_vec::Vec n a}

prefix :: (KnownNat n) => Index n -> Vec n a -> Prefix n a
prefix e v = Prefix e (zipWith (\i x -> if i<=e then x else undefined) indicesI v)

unsafeFromPrefix :: Prefix n a -> Vec n a
unsafeFromPrefix Prefix{_vec} = _vec
