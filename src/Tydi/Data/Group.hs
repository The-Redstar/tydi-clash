{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Tydi.Data.Group (
  Group(..), (:&:)(..),
  HasField(FieldType,getField,setField,_field),
  CheckField,
  module Tydi.Data.Label,
) where

import Clash.Explicit.Prelude
-- import Data.Type.Bool (If)

import Optics.Lens
import Data.Proxy (Proxy(..))
import Tydi.Data.Label (type (>::)(..))
import Shockwaves.Viewer
import Data.Typeable

-- groups
newtype Group a = Group a deriving (Generic,BitPack,NFDataX,Typeable,ShowX,Default)

infixr 5 :&:
data (:&:) l r = (:&:) l r deriving (Generic,BitPack,NFDataX,Typeable,ShowX,Default)



type family CheckField lbl a :: Bool where
  CheckField lbl (lbl >:: a) = True
  CheckField lbl ((lbl >:: a) :&: b) = True
  CheckField lbl (_ :&: b) = CheckField lbl b
  CheckField lbl (Group a) = CheckField lbl a
  CheckField lbl _ = False


class (CheckField lbl a ~ True) => HasField lbl a where
  type FieldType lbl a
  getField :: a -> FieldType lbl a
  setField :: a -> FieldType lbl a -> a
  -- OPTICS
  _field :: (HasField lbl a) => Lens a a (FieldType lbl a) (FieldType lbl a)
  _field = lens (getField @lbl) (setField @lbl)

-- label
instance (CheckField lbl (lbl >:: a) ~ True) => HasField lbl (lbl >:: a) where
  type FieldType lbl (lbl >:: a) = a
  getField (L x) = x
  setField _ = L
-- :&:
instance (
    CheckField lbl (a :&: b) ~ True,
    GroupLabel lbl (a :&: b) (CheckField lbl a)
  ) => HasField lbl (a :&: b) where
  type FieldType lbl (a :&: b) = GrpFieldType lbl (a :&: b) (CheckField lbl a)
  setField = setGroup @lbl  @(a :&: b) @(CheckField lbl a)
  getField = getGroup @lbl  @(a :&: b) @(CheckField lbl a)
-- group
instance (CheckField lbl (Group a) ~ True, HasField lbl a) => HasField lbl (Group a) where
  type FieldType lbl (Group a) = FieldType lbl a
  getField (Group x) = getField @lbl x
  setField (Group y) x = Group (setField @lbl y x)

-- class for splitting the label access of groups
class GroupLabel lbl group left where
  type GrpFieldType lbl group left
  getGroup :: group -> GrpFieldType lbl group left
  setGroup :: group -> GrpFieldType lbl group left -> group
-- left
instance (
    --CheckField lbl a ~ True
  ) => GroupLabel lbl ((lbl >:: a) :&: b) True where
  type GrpFieldType lbl ((lbl >:: a) :&: b) True = a
  getGroup ((L x) :&: _) = x
  setGroup ((L _) :&: b) x = L x :&: b
-- right
instance (
    CheckField lbl a ~ False,
    CheckField lbl b ~ True,
    HasField lbl b
  ) => GroupLabel lbl (a :&: b) False where
  type GrpFieldType lbl (a :&: b) False = FieldType lbl b
  getGroup (_ :&: x) = getField @lbl x
  setGroup (a :&: b) x = a :&: setField @lbl b x


-- BUNDLES
instance (Bundle a) => Bundle (Group a) where
  type Unbundled dom (Group a) = Group (Unbundled dom a)
  bundle (Group as) = Group <$> bundle as
  unbundle groups = Group $ unbundle ((\(Group a) -> a) <$> groups)

instance (Bundle a, Bundle b) => Bundle (a :&: b) where
  type Unbundled dom (a :&: b) = Unbundled dom a :&: Unbundled dom b
  bundle (as :&: bs) = (:&:) <$> bundle as <*> bundle bs
  unbundle groups = unbundle ((\(a :&: _) -> a) <$> groups) :&: unbundle ((\(_ :&: b) -> b) <$> groups)


-- STANDARD FUNCTIONALITY

-- eq
instance Eq a => Eq (Group a) where
  (==) (Group x) (Group y) = x==y
instance (Eq a, Eq b) => Eq (a :&: b) where
  (==) (x :&: y) (p :&: q) = (x==p) && (y==q)

-- show
instance Show a => Show (Group a) where
  show (Group x) = "Group {" <> show x <> "}"
instance (Show a, Show b) => Show (a :&: b) where
  show (x :&: y) = show x <> ", " <> show y

-- forceX -> to initialize?
-- undefGroup
-- TODO

-- autoreg
-- TODO

-- SHOCKWAVES
deriving instance (Show (Group a)) => Display (Group a)
instance (GroupSplit a) => Split (Group a) where
  structure = VICompound (groupStruct @a)
  split (Group x) _ = groupSplit x

class GroupSplit a where
  groupStruct :: [(String,VariableInfo)]
  groupSplit :: a -> [SubFieldTranslationResult]
instance (GroupSplit a, GroupSplit b) => GroupSplit (a :&: b) where
  groupStruct = groupStruct @a <> groupStruct @b
  groupSplit (x :&: y) = groupSplit x <> groupSplit y
instance (Split a, Display a, KnownSymbol lbl) => GroupSplit (lbl >:: a) where
  groupStruct = [(symbolVal $ Proxy @lbl,structure @a)]
  groupSplit (L x) = [SubFieldTranslationResult (symbolVal $ Proxy @lbl) $ translate x]
