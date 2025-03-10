module Tydi.LStream where

import Clash.Explicit.Prelude

data LStream dim (sync::SyncMode) (dir::Direction) (force::Bool) c t user dat --(c::Nat -> Nat -> Nat -> Nat -> Type -> Type -> Nat -> Type)
-- Stream(Te,t,d,s,c,r,Tu,x)
-- data, throughput, dim, sync, complexity, direction, user, force
-- LStream complexity throughput dim sync direction force user data
-- t,c in maybes to indicate parent? what to do to topmost one?

-- type Stream = New

-- common stream types
type Dim  = LStream 1 Sync    'Forward 'False
type New  = LStream 0 Sync    'Forward 'False
type Des  = LStream 0 Desync  'Forward 'False
type Flat = LStream 0 Flatten 'Forward 'False
type Rev  = LStream 0 Sync    'Reverse 'False

data SyncMode = Sync | Flatten | Desync | FlatDesync deriving (Show)
data Direction = Forward | Reverse deriving (Show)





-- optics
-- TODO


