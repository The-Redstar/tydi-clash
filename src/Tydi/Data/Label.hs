


module Tydi.Data.Label (type (>::)(..)) where

import Clash.Explicit.Prelude
import Data.Proxy

-- labels for Group/Union
infixl 7 >::
newtype (>::) l a = L a deriving (BitPack, Generic)

-- BUNDLES
instance Bundle (l >:: a) where
  type Unbundled dom (l >:: a) = l >:: Signal dom a
  bundle (L as) = L <$> as
  unbundle labels = L ((\(L a) -> a) <$> labels)

-- STANDARD FUNCTIONALITY
-- TODO

-- eq
instance Eq a => Eq (lbl >:: a) where
  (==) (L x) (L y) = x==y

-- show
instance (Show a, KnownSymbol lbl) => Show (lbl >:: a) where
  show (L x) = symbolVal (Proxy @lbl) <> " = " <> show x

-- SHOCKWAVES
-- TODO
