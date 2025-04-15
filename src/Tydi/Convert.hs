{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Tydi.Convert where-- (TydiConvertible(..),TydiConvert(..)) where

import Clash.Prelude
import Tydi.Data.Group
import Tydi.Internal.Data.Union
--import qualified Data.Convertible
import GHC.Generics
import Data.Type.Equality (type (==))
import Data.Coerce (coerce, Coercible)

class DataConvertible a b where -- import doesnt work :(
  convert# :: a -> b




-- CONVERSION BETWEEN ANY TWO TYPES

newtype IsTydiType a = ITT a
newtype IsNotTydiType a = INTT a

type family CheckTydiType a where
  CheckTydiType (Group a) = IsTydiType (Group a)
  CheckTydiType (Union a) = IsTydiType (Union a)
  CheckTydiType a         = IsNotTydiType a

class (TydiConvertible' (CheckTydiType a) (CheckTydiType b) (a == b)) => TydiConvertible a b where
  convert :: a -> b
instance (TydiConvertible' (CheckTydiType a) (CheckTydiType b) (a == b), Coercible a (CheckTydiType a), Coercible (CheckTydiType b) b) => TydiConvertible a b where
  convert x = coerce @(CheckTydiType b) @b $ convert' @(CheckTydiType a) @(CheckTydiType b) @(a==b) $ coerce @a @(CheckTydiType a) x

-- helper class for
class TydiConvertible' a b eq where
  convert' :: a -> b
-- a -> a
instance TydiConvertible' (IsTydiType a) (IsTydiType a) True where
  convert' = id
instance TydiConvertible' (IsNotTydiType a) (IsNotTydiType a) True where
  convert' = id

-- a!T -> b!T
instance (DataConvertible a b) => TydiConvertible' (IsNotTydiType a) (IsNotTydiType b) False where
  convert' (INTT x) = INTT $ convert# x
-- aT -> bT
instance (TydiConvertible'' a b) => TydiConvertible' (IsTydiType a) (IsTydiType b) False where
  convert' (ITT x) = ITT $ convert'' x
-- a!T -> bT
instance (TydiConvert a,TydiConvertible (TydiRep a) b) => TydiConvertible' (IsNotTydiType a) (IsTydiType b) False where
  convert' (INTT x) = ITT $ convert (toTydi x)
-- aT -> b!T
instance (TydiConvert b,TydiConvertible a (TydiRep b)) => TydiConvertible' (IsTydiType a) (IsNotTydiType b) False where
  convert' (ITT x) = INTT $ fromTydi (convert x)




-- CONVERSION BETWEEN TYDI TYPES

-- TODO: allow conversion between Group (lbl >:: x), Union (lbl >:: x), and x

-- helper class for dealing with special
class TydiConvertible'' a b where
  convert'' :: a -> b

-- labels
instance (TydiConvertible a b) => TydiConvertible'' (lbl >:: a) (lbl' >:: b) where
  convert'' (L x) = L $ convert x

--groups
instance (TydiConvertible'' a b) => TydiConvertible'' (Group a) (Group b) where
  convert'' (Group x) = Group (convert'' x)
instance (TydiConvertible'' a b,TydiConvertible'' c d) => TydiConvertible'' (a :&: c) (b :&: d) where
  convert'' (x :&: y) = convert'' x :&: convert'' y

--unions
instance (UnionUnionConvert (Union a) a (Union b) b 0) => TydiConvertible'' (Union a) (Union b) where
  convert'' = unionConvert @(Union a) @a @(Union b) @b @0

-- TODO: union
class (UnionCount a ~ UnionCount b) => UnionUnionConvert a x b y (n::Nat) where
  unionConvert :: a -> b
instance (
    UnionCount a ~ UnionCount b
  , KnownNat (UnionWidth a)
  , KnownNat (UnionWidth b)
  , BitPack x
  , BitPack y
  , n+1 <= UnionCount a
  , TydiConvertible x y
  ) => UnionUnionConvert (Union a) (lbl >:: x) (Union b) (lbl' >:: y) n where
  unionConvert Union{tag,union} = Union{tag,union = resize $ pack @y $ convert $ unpack @x $ resize union}
instance (
    UnionCount a ~ UnionCount b
  , UnionUnionConvert (Union a) p (Union b) q n
  , UnionUnionConvert (Union a) r (Union b) s (n+1)
  , n+1 <= UnionCount a
  , KnownNat n
  , KnownNat (UnionCount b)
  ) => UnionUnionConvert (Union a) (p :|: r) (Union b) (q :|: s) n where
  unionConvert u@Union{tag} = if tag == fromSNat (SNat @n) then
                                unionConvert @(Union a) @p @(Union b) @q @n u
                              else
                                unionConvert @(Union a) @r @(Union b) @s @(n+1) u


-- CONVERSION BETWEEN HASKELL AND TYDI TYPES


-- flatten tydi/generics structure, since the generic structure is balanced instead
class Flatten a where
  type Flattened a
  flatten :: a -> Flattened a
  unflatten :: Flattened a -> a

-- tydi groups (and labels)
instance Flatten (lbl >:: a) where
  type Flattened (lbl >:: a) = lbl >:: a
  flatten = id
  unflatten = id

instance (Flatten a) => Flatten (Group a) where
  type Flattened (Group a) = Group (Flattened a)
  flatten (Group a) = Group (flatten a)
  unflatten (Group a) = Group (unflatten a)
instance (Flatten (a :&: b :&: c)) => Flatten ((a :&: b) :&: c) where
  type Flattened ((a :&: b) :&: c) = Flattened (a :&: b :&: c)
  flatten ((a :&: b) :&: c) = flatten (a :&: b :&: c)
  unflatten x = (a :&: b) :&: c
    where (a :&: b :&: c) = unflatten @(a :&: b :&: c) x
instance (Flatten b) => Flatten ((lbl >:: a) :&: b) where
  type Flattened ((lbl >:: a) :&: b) = (lbl >:: a) :&: Flattened b
  flatten (L a :&: b) = L a :&: flatten b
  unflatten (L a :&: b) = L a :&: unflatten b

-- generic sum types
instance Flatten (M1 x y z p) where
  type Flattened (M1 x y z p) = (M1 x y z p)
  flatten = id
  unflatten = id
instance (Flatten ((a :+: (b :+: c)) p)) => Flatten (((a :+: b) :+: c) p) where
  type Flattened (((a :+: b) :+: c) p) = Flattened ((a :+: (b :+: c)) p)
  flatten (L1 (L1 a)) = flatten @((a :+: (b :+: c)) p) $ L1     a
  flatten (L1 (R1 b)) = flatten @((a :+: (b :+: c)) p) $ R1 (L1 b)
  flatten (R1     c ) = flatten @((a :+: (b :+: c)) p) $ R1 (R1 c)
  unflatten x = case unflatten x of
    L1     a  -> L1 (L1 a)
    R1 (L1 b) -> L1 (R1 b)
    R1 (R1 c) -> R1     c
instance (Flatten (b p), WithoutP (Flattened (b p)) p ~ Flattened (b p)) => Flatten ((C1 meta fields :+: b) p) where
  type Flattened ((C1 meta fields :+: b) p) = (C1 meta fields :+: WithoutP (Flattened (b p))) p -- uh oh TODO
  flatten (L1 a ) = L1 a
  flatten (R1 b) = R1 $ flatten b
  unflatten (L1 a) = L1 a
  unflatten (R1 b) = R1 $ unflatten b

type family WithoutP a where -- p hack
  WithoutP (f a) = f


-- numbering missing labels
type family NatToSymbol (n :: Nat) :: Symbol where
  NatToSymbol 0 = "0"
  NatToSymbol 1 = "1"
  NatToSymbol 2 = "2"
  NatToSymbol 3 = "3"
  NatToSymbol 4 = "4"
  NatToSymbol 5 = "5"
  NatToSymbol 6 = "6"
  NatToSymbol 7 = "7"
  NatToSymbol 8 = "8"
  NatToSymbol 9 = "9"
  NatToSymbol n = AppendSymbol (NatToSymbol (Div n 10)) (NatToSymbol (Mod n 10))

class NumberLabels (n::Nat) a where
  type Numbered n a
  number   :: a -> Numbered n a
  denumber :: Numbered n a -> a
instance (NumberLabels n a, NumberLabels (n+1) b) => NumberLabels n (a :&: b) where
  type Numbered n (a :&: b) = Numbered n a :&: Numbered (n+1) b
  number   (a :&: b) = number @n a :&: number @(n+1) b
  denumber (a :&: b) = denumber @n a :&: denumber @(n+1) b
instance (NumberLabels' n (lbl >:: a) (lbl=="")) => NumberLabels n (lbl >:: a) where
  type Numbered n (lbl >:: a) = Numbered' n (lbl >:: a) (lbl=="")
  number   = number' @n @(lbl >:: a) @(lbl=="")
  denumber = denumber' @n @(lbl >:: a) @(lbl=="")

class NumberLabels' (n::Nat) l (need::Bool) where
  type Numbered' n l need
  number'   :: l -> Numbered' n l need
  denumber' :: Numbered' n l need -> l
instance NumberLabels' n (lbl >:: a) False where
  type Numbered' n (lbl >:: a) False = lbl >:: a
  number'   (L x) = L x
  denumber' (L x) = L x
instance NumberLabels' n ("" >:: a) True where
  type Numbered' n ("" >:: a) True = AppendSymbol "field" (NatToSymbol n) >:: a
  number'   (L x) = L x
  denumber' (L x) = L x

-- main conversion class
class TydiConvert a where
  type TydiRep a :: Type
  type TydiRep a = GTydiRep (Rep a ())

  toTydi :: a -> TydiRep a
  default toTydi :: (Generic a, AutoTydiConvert (Rep a ()), GTydiRep (Rep a ()) ~ TydiRep a) => a -> TydiRep a
  toTydi x = toTydi' (from @a @() x)

  fromTydi :: TydiRep a -> a
  default fromTydi :: (Generic a, AutoTydiConvert (Rep a ()), GTydiRep (Rep a ()) ~ TydiRep a) => TydiRep a -> a
  fromTydi x = to @a $ fromTydi' @(Rep a ()) x


class AutoTydiConvert a where
  type GTydiRep a
  toTydi' :: a -> GTydiRep a
  fromTydi' :: GTydiRep a -> a


-- single constructor
-- T C -> Constr C
instance (AutoTydiConstrConvert (C1 (MetaCons name x y) fields p)) => AutoTydiConvert (D1 meta (C1 (MetaCons name x y) fields) p) where
  type GTydiRep (D1 meta (C1 (MetaCons name x y) fields) p) = GTydiRepC (C1 (MetaCons name x y) fields p)
  toTydi' (M1 x) = toTydiC x
  fromTydi' x = M1 $ fromTydiC @(C1 (MetaCons name x y) fields p) x

-- multiple constructors
-- T (a + b) -> UnionC (a+b) (UnionRep (a+b)) 0
instance (
    AutoTydiConvert (Flattened ((a :+: b) p))
  , 1 <= UnionCount (GTydiRep (Flattened ((a :+: b) p)))
  , Flatten ((a :+: b) p)
  , GenericToUnionConvert (Flattened ((:+:) a b p)) (Union (GTydiRep (Flattened ((:+:) a b p)))) 0
  ) => AutoTydiConvert (D1 meta (a :+: b) p) where
  type GTydiRep (D1 meta (a :+: b) p) = Union (GTydiRep (Flattened ((a :+: b) p)))
  toTydi' (M1 x) = unionFromGeneric @(Flattened ((a :+: b) p)) @(Union (GTydiRep (Flattened ((a :+: b) p)))) @0 $ flatten x
  fromTydi' x = M1 $ unflatten $ genericFromUnion @(Flattened ((a :+: b) p)) @(Union (GTydiRep (Flattened ((a :+: b) p)))) @0 x
-- (a + b)
instance (AutoTydiConvert (a p), AutoTydiConvert (b p)) => AutoTydiConvert ((a :+: b) p) where -- only for GTydiRep
  type GTydiRep ((a :+: b) p) = (GTydiRep (a p) :|: GTydiRep (b p))
  toTydi' = undefined
  fromTydi' = undefined
-- C_lbl -> lbl :: Constr C_lbl
instance (AutoTydiConstrConvert (C1 (MetaCons cname x0 x1) fields p)) => AutoTydiConvert (C1 (MetaCons cname x0 x1) fields p) where -- only for GTydiRep
  type GTydiRep (C1 (MetaCons cname x0 x1) fields p) = cname >:: GTydiRepC (C1 (MetaCons cname x0 x1) fields p)
  toTydi' = undefined
  fromTydi' = undefined

-- UnionC
class GenericToUnionConvert a u (n::Nat) where
  unionFromGeneric :: a -> u
  genericFromUnion :: u -> a
-- UnionC (a + b) u n -> Union{n,Constr a} / UnionC b u (n+1)
instance (
    n+1 <= UnionCount u
  , KnownNat n
  , KnownNat (UnionCount u)
  , KnownNat (UnionWidth u)
  , BitPack (GTydiRepC (constr p))
  , AutoTydiConstrConvert (constr p)
  , GenericToUnionConvert (b p) (Union u) (1 + n)
  ) => GenericToUnionConvert ((constr :+: b) p) (Union u) n where
  unionFromGeneric (L1 x) = Union{tag=fromSNat $ SNat @n,union=resize $ pack $ toTydiC x}
  unionFromGeneric (R1 x) = unionFromGeneric @(b p) @(Union u) @(n+1) x
  genericFromUnion u@Union{tag,union} = if tag==(fromSNat $ SNat @n) then
                                          L1 $ fromTydiC @(constr p) $ unpack $ resize union
                                        else
                                          R1 $ genericFromUnion @(b p) @(Union u) @(n+1) u
-- UnionC C u n -> Union{n,Constr C}
instance (n+1 <= UnionCount u, KnownNat n, KnownNat (UnionCount u), KnownNat (UnionWidth u), BitPack (GTydiRepC (C1 meta fields p)), AutoTydiConstrConvert (C1 meta fields p)) => GenericToUnionConvert (C1 meta fields p) (Union u) n where
  unionFromGeneric x = Union{tag=fromSNat $ SNat @n :: Index (UnionCount u),union=resize $ pack $ toTydiC x}
  genericFromUnion Union{union} = fromTydiC @(C1 meta fields p) $ unpack $ resize union

-- Constructors
class AutoTydiConstrConvert a where
  type GTydiRepC a
  toTydiC :: a -> GTydiRepC a
  fromTydiC :: GTydiRepC a -> a

-- no fields
-- C U -> ()
instance AutoTydiConstrConvert (C1 meta U1 p) where
  type GTydiRepC (C1 meta U1 p) = ()
  toTydiC _ = ()
  fromTydiC _ = M1 U1

-- one field
-- C S -> Group Constr S
instance (
    AutoTydiConstrConvert (S1 meta' val p)
  , NumberLabels 0 (GTydiRepC (S1 meta' val p))
  ) => AutoTydiConstrConvert (C1 meta (S1 meta' val) p) where
  type GTydiRepC (C1 meta (S1 meta' val) p) = Group (Numbered 0 (GTydiRepC (S1 meta' val p)))
  toTydiC (M1 x) = Group $ number @0 @(GTydiRepC (S1 meta' val p)) $ toTydiC x
  fromTydiC (Group x) = M1 $ fromTydiC @(S1 meta' val p) $ denumber @0 @(GTydiRepC (S1 meta' val p)) x

-- multiple fields
-- C (a * b) -> Group Constr (a * b)
instance (
    AutoTydiConstrConvert ((a :*: b) p)
  , Flatten (GTydiRepC (a p) :&: GTydiRepC (b p))
  , NumberLabels 0 (Flattened (GTydiRepC ((a :*: b) p)))
  ) => AutoTydiConstrConvert (C1 meta (a :*: b) p) where
  type GTydiRepC (C1 meta (a :*: b) p) = Group (Numbered 0 (Flattened (GTydiRepC ((a :*: b) p))))
  toTydiC (M1 x) = Group $ number @0 @(Flattened (GTydiRepC ((a :*: b) p))) $ flatten $ toTydiC x
  fromTydiC (Group x) = M1 $ fromTydiC @((a :*: b) p) $ unflatten $ denumber @0 @(Flattened (GTydiRepC ((a :*: b) p))) x
-- (a * b) -> (a & b)
instance (AutoTydiConstrConvert (a p),AutoTydiConstrConvert (b p)) => AutoTydiConstrConvert ((a :*: b) p) where
  type GTydiRepC ((a :*: b) p) = GTydiRepC (a p) :&: GTydiRepC (b p)
  toTydiC (a :*: b) = toTydiC a :&: toTydiC b
  fromTydiC (a :&: b) = fromTydiC @(a p) a :*: fromTydiC @(b p) b

-- a field
-- Sx
instance AutoTydiConstrConvert (S1 (MetaSel (Just fieldName) a b c) (Rec0 f) p) where
  type GTydiRepC (S1 (MetaSel (Just fieldName) a b c) (Rec0 f) p) = fieldName >:: f
  toTydiC (M1 (K1 x)) = L x
  fromTydiC (L x) = M1 (K1 x)
instance AutoTydiConstrConvert (S1 (MetaSel Nothing a b c) (Rec0 f) p) where
  type GTydiRepC (S1 (MetaSel Nothing a b c) (Rec0 f) p) = "" >:: f
  toTydiC (M1 (K1 x)) = L x
  fromTydiC (L x) = M1 (K1 x)

-- instance for isomorphic types
-- instance for increasing complexity levels

-- instance for decreasing ready signal complexity level
{-


do a derive like Shockwaves Split

M1 D meta (C1 ...) -> single constructor -> get constructor
  data T = X... -> GTydiRep X
M1 D meta (a :+: b) -> multiple constructors; union
  data T = X... | Y... -> Union (Flatten (GTydiRep X...|Y...))
C1 (MetaCons constrName a b) U1 -> no fields -> ()
  X -> ()
C1 (MetaCons constrName a b) (S1 ...) -> single field; use group or not?
  X a -> Group (Flatten (GTydiRep a))
C1 (MetaCons constrName a b) (a :*: b) -> multiple fields -> Group
  X a&b -> Group (Flatten (GTydiRep a))
S1 (MetaSel (Some fieldName) a b c) (Rec0 f) -> fieldName >:: f
  lbl::val -> lbl >:: val
S1 (MetaSel Nothing a b c) (Rec0 f) -> (counter) >:: f
  val -> field0 :: val

data T = A | B Int Int
type Rep T () =
  M1
    D
    (MetaData "T" "Ghci3" "interactive" False)
    (C1 (MetaCons "A" PrefixI False) U1
      :+: C1
            (MetaCons "B" PrefixI False)
            (S1
              (MetaSel
                  Nothing
                  NoSourceUnpackedness
                  NoSourceStrictness
                  DecidedLazy)
              (Rec0 Int)
            :*: S1
                  (MetaSel
                      Nothing
                      NoSourceUnpackedness
                      NoSourceStrictness
                      DecidedLazy)
                  (Rec0 Int)))
    ()
-}



-- A BUNCH OF STANDARD IMPLEMENTATIONS FOR TYDI REPS
-- TODO
-- Mostly Vector
