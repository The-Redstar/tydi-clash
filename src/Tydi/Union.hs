{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}



module Tydi.Union where

import Clash.Explicit.Prelude
import Tydi.Label ( type (>::) )
-- import Data.Proxy
import Optics.Prism
import Data.Proxy
-- import GHC.TypeLits (natSing)

-- unions
data Union a = Union{
    tag::Index (UnionCount a),
    union:: BitVector (UnionWidth a)
  } deriving (Generic)
-- deriving instance (KnownNat (UnionCount a), KnownNat (UnionWidth a)) => Show (Union a)
deriving instance (KnownNat (UnionCount a), KnownNat (UnionWidth a), 1 <= UnionCount a) => BitPack (Union a)

infixr 6 :|:
data (:|:) l r

type family UnionCount x :: Nat where
  UnionCount (_ >:: _) = 1
  UnionCount (a :|: b) = UnionCount a + UnionCount b

type family UnionWidth x :: Nat where
  UnionWidth (_ >:: v) = BitSize v
  UnionWidth (a :|: b) = Max (UnionWidth a) (UnionWidth b)


-- GET/SET

type family CheckVariant lbl a :: Bool where
  CheckVariant lbl (lbl >:: _) = True
  CheckVariant lbl ((lbl >:: _) :|: b) = True
  CheckVariant lbl (_ :|: b) = CheckVariant lbl b
  CheckVariant lbl (Union a) = CheckVariant lbl a
  CheckVariant lbl _ = False


type family LabelIndex lbl (n::Nat) a  :: Nat where
  LabelIndex lbl n (lbl >:: _) = n
  LabelIndex lbl n ((lbl >:: _) :|: _) = n
  LabelIndex lbl n (_ :|: b) = LabelIndex lbl (n+1) b

type family VariantType lbl a where
  VariantType lbl (lbl >:: a) = a
  VariantType lbl ((lbl >:: a) :|: _) = a
  VariantType lbl (_ :|: b) = VariantType lbl b
  VariantType lbl (Union a) = VariantType lbl a


class (CheckVariant lbl a ~ True) => HasVariant lbl a where
  getVariant :: a -> Maybe (VariantType lbl a)
  mkVariant :: VariantType lbl a -> a
  -- OTPICS
  _variant :: Prism' a (VariantType lbl a)
  _variant = prism (mkVariant @lbl @a) matcher --TODO
    where matcher x = case getVariant @lbl x of
            Nothing -> Left x
            Just p  -> Right p

-- union
instance (
    -- HasVariant lbl a,
    CheckVariant lbl a ~ True,
    (LabelIndex lbl 0 a + 1) <= UnionCount a,
    KnownNat (LabelIndex lbl 0 a),
    KnownNat (UnionCount a),
    KnownNat (UnionWidth a),
    BitPack (VariantType lbl a)
  ) => HasVariant lbl (Union a) where
  getVariant Union{tag,union} = if i == tag then Just $ unpack $ resize union else Nothing
    where i = fromSNat $ SNat @(LabelIndex lbl 0 a)
  mkVariant x = Union{tag=i, union=bits}
    where
      i :: Index (UnionCount a)
      i = fromSNat $ SNat @(LabelIndex lbl 0 a)
      bits = resize $ pack x

-- OPTICS
-- TODO


-- ISOMORPHICS
-- TODO



-- BUNDLES
instance Bundle (Union a) -- like Maybe, just use default implementation (i.e. not unbundleable)


-- STANDARD FUNCTIONALITY
-- TODO

-- eq
instance (UEq a 0 (UnionCount a) (UnionWidth a), KnownNat (UnionCount a)) => Eq (Union a) where
  (==) Union{tag,union} Union{tag=tag',union=union'} = (tag==tag') && ueq @a @0 @(UnionCount a) @(UnionWidth a) tag union union'

class (KnownNat n) => UEq a (i::Nat) (n::Nat) (w::Nat) where
  ueq :: Index n -> BitVector w -> BitVector w -> Bool
instance (Eq a, BitPack a, KnownNat i, KnownNat n, KnownNat w, i+1<=n) => UEq (lbl >:: a) i n w where
  ueq i a b = (i==fromSNat (SNat @i)) && (unpack @a (resize a)==unpack @a (resize b))
instance (UEq a i n w, UEq b (i+1) n w) => UEq (a :|: b) i n w where
  ueq i a b = ueq @a @i i a b || ueq @b @(i+1) i a b

-- show
instance (UShow a 0 (UnionCount a) (UnionWidth a)) => Show (Union a) where
  show Union{tag,union} = "Union {" <> ushow @a @0 @(UnionCount a) @(UnionWidth a) tag union <> "}"

class UShow a (i::Nat) (n::Nat) (w::Nat) where
  ushow :: Index n -> BitVector w -> String
instance (KnownNat w,KnownNat n,KnownNat i,BitPack a,Show a,i+1<=n,KnownSymbol lbl) => UShow (lbl >:: a) i n w where
  ushow i x = if i==fromSNat (SNat @i) then symbolVal (Proxy @lbl) <> " = " <> (show $ unpack @a (resize x)) else ""
instance (UShow a i n w, UShow b (i+1) n w, i+1<=n,KnownNat n, KnownNat i) => UShow (a :|: b) i n w where
  ushow i x = if i==fromSNat (SNat @i) then ushow @a @i @n @w i x else ushow @b @(i+1) @n @w i x

-- autoreg
-- TODO


-- SHOCKWAVES
-- TODO
