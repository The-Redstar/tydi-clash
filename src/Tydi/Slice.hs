{-# LANGUAGE CPP #-}


module Tydi.Slice (Slice, slice,start,end,strobed,unsafeFromSlice) where
import Clash.Explicit.Prelude hiding (slice)

-- closed interval, non-empty slice of a vector

data Slice n a = Slice{start::Index n,end::Index n,_vec::Vec n a}

slice :: (KnownNat n) => Index n -> Index n -> Vec n a -> Slice n a
slice s e v = Slice s e (zipWith (\i x -> if s<=i && i<=e then x else undefined) indicesI v)

strobed :: KnownNat n => Slice n a -> Vec n (Maybe a)
strobed (Slice s e v) = zipWith (\i x -> if s<=i && i<=e then Just x else Nothing) indicesI v

unsafeFromSlice :: Slice n a -> Vec n a
unsafeFromSlice Slice{_vec} = _vec

-- full -> slice 0 (n-1)
-- align -> aligns slice so start is at 0
-- sliceLength -> (e-s+1)

-- define !!, head, last, map, zipWithX, zipX, unzipX,foldr,foldl,foldr1,foldl1,fold, replace,reverse,length
-- bundle, unbundle

#define CONS_PREC 5

instance (Show a,KnownNat n) => Show (Slice n a) where
  showsPrec n Slice{start,end,_vec} = case _vec of
    Nil -> showString "Nil"
    vs -> showParen (n > CONS_PREC) (go 0 vs)

   where
    go :: Index n -> Vec m a -> ShowS
    go _ Nil = showString "Nil"
    go i (x `Cons` xs) =
        (if start<=i && i<=end then showsPrec (CONS_PREC + 1) x else showString "-")
      . showString " :> "
      . go (i+1) xs

instance (ShowX a,KnownNat n) => ShowX (Slice n a) where
  showsPrecX n vs =
    case isX vs of
      Right Slice{_vec=Nil} -> showString "Nil"
      Left _ -> showString "undefined"
      Right Slice{start,end,_vec} -> showParen (n > CONS_PREC) (go start end 0 _vec)
   where
    go :: Index n -> Index n -> Index n -> Vec m a -> ShowS
    go _ _ _ (isX -> Left _) = showString "undefined"
    go _ _ _ Nil = showString "Nil"
    go start end i (x `Cons` xs) =
        (if start<=i && i<=end then showsPrecX (CONS_PREC + 1) x else showString "-")
      . showString " :> "
      . go start end (i+1) xs
