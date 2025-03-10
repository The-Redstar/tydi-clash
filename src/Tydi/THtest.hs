
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Tydi.THtest where

import Tydi.Data
import Tydi.LStream
import Tydi.Internal.PStream
import Tydi.Synthesis


import Clash.Explicit.Prelude
import Data.Data (Proxy(..))
import Language.Haskell.TH

-- main :: IO ()
-- main = do
--   print $ runQ [|LStream|]
--   print $ runQ [t|LStream|]



data LStreamTH e = LStream{dim::Natural,sync::SyncMode, dir::Direction,force::Bool, c::Natural, t::Natural, user::Q Type, dat::e}

d :: Q Type -> Q Type
d = id

class TydiSynthesizable a where
  synthBundle :: a -> Q Type
  synthData   :: a -> Q Type

instance (TydiSynthesizable e) => TydiSynthesizable (LStreamTH e) where
  synthBundle LStream{dim,sync,dir,force,c,t,user,dat} = [t|StreamNode (PStream C8 $(return $ LitT (NumTyLit $ fromIntegral n)) $(return $ LitT (NumTyLit $ fromIntegral dim)) $user $(synthData dat) 'True) $(synthBundle dat)|]
    where n = t
  synthData _ = [t|()|]

instance TydiSynthesizable (Q Type) where
  synthBundle _ = [t|()|]
  synthData   x = x

instance (TydiSynthesizable a) => TydiSynthesizable (Group a) where
  synthBundle (Group x) = [t|Group $(synthBundle x)|]
  synthData   (Group x) = [t|Group $(synthData   x)|]

instance (TydiSynthesizable a, TydiSynthesizable b) => TydiSynthesizable (a :*: b) where
  synthBundle (l :*: r) = [t|$(synthBundle l) :*: $(synthBundle r)|]
  synthData   (l :*: r) = [t|$(synthData   l) :*: $(synthData   r)|]

instance (TydiSynthesizable a, KnownSymbol l) => TydiSynthesizable (L l a) where
  synthBundle (L x) = [t|L $(return $ LitT (StrTyLit $ symbolVal $ Proxy @l)) $(synthBundle x)|]
  synthData   (L x) = [t|L $(return $ LitT (StrTyLit $ symbolVal $ Proxy @l)) $(synthData   x)|]
