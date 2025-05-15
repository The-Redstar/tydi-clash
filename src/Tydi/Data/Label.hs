


module Tydi.Data.Label (type (>::)(..)) where

import Clash.Explicit.Prelude
import Data.Proxy
import Data.Typeable

-- labels for Group/Union
infixl 7 >::
newtype (>::) l a = L a deriving (BitPack,Generic,NFDataX,Eq,Typeable,ShowX,Default)

-- BUNDLES
instance Bundle (l >:: a) where
  type Unbundled dom (l >:: a) = l >:: Signal dom a
  bundle (L as) = L <$> as
  unbundle labels = L ((\(L a) -> a) <$> labels)

-- STANDARD FUNCTIONALITY
-- TODO

-- -- eq
-- instance Eq a => Eq (lbl >:: a) where
--   (==) (L x) (L y) = x==y

-- show
instance (Show a, KnownSymbol lbl) => Show (lbl >:: a) where
  show (L x) = symbolVal (Proxy @lbl) <> " = " <> show x

-- default
-- instance (Default a) => Default (lbl >:: a) where
--   def = L def
