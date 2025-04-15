

module Tydi.Data.Union (
  Union, (:|:),
  HasVariant(getVariant,mkVariant,_variant),VariantType,
  CheckVariant,
  module Tydi.Data.Label,
) where

import Tydi.Internal.Data.Union
import qualified Tydi.Data.Label
