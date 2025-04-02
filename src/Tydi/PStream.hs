
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Tydi.PStream where
--import Tydi.Internal.PStreamRead (getUser, Stai (getStaiExt), Data (getDataSliced), getStrbRaw, Strb (getStrbExtRaw))
import Clash.Explicit.Prelude hiding (last,slice)
import qualified Clash.Explicit.Prelude
import Data.Type.Bool (If)
import Tydi.Slice (Slice,slice)
import qualified Tydi.Slice as Slice
import Tydi.Prefix (Prefix,prefix)
import qualified Tydi.Prefix as Prefix
import Data.Maybe (fromMaybe, isJust, fromJust)
import Clash.Sized.Internal.BitVector (xToBV)

data PStream c n d u e where
  PStream
    :: (CompleteComplexity c n d u e)
    => { valid :: Bool
       , dat   :: Vec n e
       , user  :: u
       , last  :: LastType (PStreamTransfer c n d u e)
       , stai  :: StaiType (PStreamTransfer c n d u e)
       , endi  :: Index n
       , strb  :: StrbType (PStreamTransfer c n d u e)
       }
    -> PStream c n d u e

-- type PStream c n d u e = PStream' (PStreamTransfer c n d u e)

data PStreamTransfer c n d u e where
  PSTransfer
    :: (CompleteComplexity c n d u e)
    => { dat   :: Vec n e
       , user  :: u
       , last  :: LastType (PStreamTransfer c n d u e)
       , stai  :: StaiType (PStreamTransfer c n d u e)
       , endi  :: Index n
       , strb  :: StrbType (PStreamTransfer c n d u e)
       }
    -> PStreamTransfer c n d u e

class ( StaiDep (PStreamTransfer c n d u e) (HasStai c)
      , StrbDep (PStreamTransfer c n d u e) (HasMultiStrb c)
      , LastDep (PStreamTransfer c n d u e) (HasMultiLast c)
      , DataFunc (PStreamTransfer c n d u e) (HasStai c) (HasMultiStrb c)
      , KnownNat n
      , KnownNat d
      --, n~n0+1
      ) => CompleteComplexity c n d u e
instance ( StaiDep (PStreamTransfer c n d u e) (HasStai c)
      , StrbDep (PStreamTransfer c n d u e) (HasMultiStrb c)
      , LastDep (PStreamTransfer c n d u e) (HasMultiLast c)
      , DataFunc (PStreamTransfer c n d u e) (HasStai c) (HasMultiStrb c)
      , KnownNat n
      , KnownNat d
      -- , n~n0+1
      ) => CompleteComplexity c n d u e


-- TODO: patterns for treating PStream as a Maybe, which would also allow for functor and monad

-- complexity levels
data Complexity = C Nat | CInherit

type family HasStai c :: Bool where
  HasStai (C 6) = True
  HasStai (C 7) = True
  HasStai (C 8) = True
  HasStai (C _) = False

type family HasMultiStrb c :: Bool where
  HasMultiStrb (C 7) = True
  HasMultiStrb (C 8) = True
  HasMultiStrb (C _) = False

type family HasMultiLast c :: Bool where
  HasMultiLast (C 8) = True
  HasMultiLast (C _) = False


-- SIGNAL TYPES AND PARAMETERS

type family DataType pstream where
  DataType (PStream c n d u e) = e
  DataType (PStreamTransfer c n d u e) = DataType (PStream c n d u e)

type family UserType pstream where
  UserType (PStream c n d u e) = u
  UserType (PStreamTransfer c n d u e) = UserType (PStream c n d u e)

type family Lanes pstream where
  Lanes (PStream c n d u e) = n
  Lanes (PStreamTransfer c n d u e) = Lanes (PStream c n d u e)

type family Dims pstream where
  Dims (PStream c n d u e) = d
  Dims (PStreamTransfer c n d u e) = Dims (PStream c n d u e)


type family StaiType pstream where
  StaiType (PStream c n d u e) = If (HasStai c) (Index n) ()
  StaiType (PStreamTransfer c n d u e) = StaiType (PStream c n d u e)

type family StrbType pstream where
  StrbType (PStream c n d u e) = If (HasMultiStrb c) (Vec n Bool) Bool
  StrbType (PStreamTransfer c n d u e) = StrbType (PStream c n d u e)

type family LastType pstream where
  LastType (PStream c n d u e) = If (HasMultiLast c) (Vec n (Vec d Bool)) (Vec d Bool)
  LastType (PStreamTransfer c n d u e) = LastType (PStream c n d u e)

type StaiType' c n   = StaiType (PStream c n 0 () ())
type StrbType' c n   = StrbType (PStream c n 0 () ())
type LastType' c n d = LastType (PStream c n d () ())


type family SliceType pstream where
  SliceType (PStream c n d u e) = If (HasStai c) (Slice n e) (Prefix n e)
  SliceType (PStreamTransfer c n d u e) = SliceType (PStream c n d u e)

type family SliceStrbType pstream where
  SliceStrbType (PStream c n d u e) = If (HasMultiStrb c) (SliceType' c n (Maybe e)) (Maybe (SliceType' c n e))
  SliceStrbType (PStreamTransfer c n d u e) = SliceStrbType (PStream c n d u e)

type SliceType' c n e = SliceType (PStream c n 0 () e)
type SliceStrbType' c n e = SliceStrbType (PStream c n 0 () e)

-- GETTERS / a few setter thingies too

getTransfer :: PStream c n d u e -> Maybe (PStreamTransfer c n d u e)
getTransfer p@PStream{valid=True} = Just $ unsafeGetTransfer p
getTransfer _ = Nothing
unsafeGetTransfer :: PStream c n d u e -> PStreamTransfer c n d u e
unsafeGetTransfer PStream{dat,user,last,stai,endi,strb} = PSTransfer{dat,user,last,stai,endi,strb}

-- valid
isValid :: PStream c n d u e -> Bool
isValid PStream{valid} = valid

-- data
getDataRaw :: PStreamTransfer c n d u e -> Vec n e
getDataRaw PSTransfer{dat} = dat

class DataFunc p bstai bstrb where
  getDataSliced' :: p -> SliceStrbType p
  getDataStrobed' :: p -> Vec (Lanes p) (Maybe (DataType p))
instance (HasStai c ~ False, HasMultiStrb c ~ False) => DataFunc (PStreamTransfer c n d u e) False False where
  getDataSliced' PSTransfer{dat,endi,strb} = fromBool strb $ prefix endi dat
  getDataStrobed' PSTransfer{dat,endi,strb} = zipWith fromBool (maskRange 0 endi (repeat strb)) dat
instance (HasStai c ~ True, HasMultiStrb c ~ False) => DataFunc (PStreamTransfer c n d u e) True False where
  getDataSliced' PSTransfer{dat,stai,endi,strb} = fromBool strb $ slice stai endi dat
  getDataStrobed' PSTransfer{dat,stai,endi,strb} = zipWith fromBool (maskRange stai endi (repeat strb)) dat
instance (HasStai c ~ True, HasMultiStrb c ~ True) => DataFunc (PStreamTransfer c n d u e) True True where
  getDataSliced' PSTransfer{dat,stai,endi,strb} = slice stai endi $ zipWith fromBool strb dat
  getDataStrobed' PSTransfer{dat,stai,endi,strb} = zipWith fromBool (maskRange stai endi strb) dat


getDataSliced :: PStreamTransfer c n d u e -> SliceStrbType (PStreamTransfer c n d u e)
getDataSliced (p@PSTransfer{}::PStreamTransfer c n d u e) = getDataSliced' @(PStreamTransfer c n d u e) @(HasStai c) @(HasMultiStrb c) p
getDataStrobed :: PStreamTransfer c n d u e -> Vec n (Maybe e)
getDataStrobed (p@PSTransfer{}::PStreamTransfer c n d u e) = getDataStrobed' @(PStreamTransfer c n d u e) @(HasStai c) @(HasMultiStrb c) p


-- user
getUser :: PStreamTransfer c n d u e -> u
getUser PSTransfer{user} = user

-- last
getLast :: PStreamTransfer c n d u e -> LastType (PStream c n d u e)
getLast PSTransfer{last} = last

class LastDep p b where
  getLastExt' :: p -> Vec (Lanes p) (Vec (Dims p) Bool)
  mkLast :: Vec (Lanes p) (Vec (Dims p) Bool) -> LastType p
instance (HasMultiLast c ~ False, n~n0+1) => LastDep (PStreamTransfer c n d u e) False where
  getLastExt' PSTransfer{last} = reverse $ last :> repeat (repeat False)
  mkLast = Clash.Explicit.Prelude.last
instance (HasMultiLast c ~ True) => LastDep (PStreamTransfer c n d u e) True where
  getLastExt' PSTransfer{last} = last
  mkLast = id

getLastExt :: PStreamTransfer c n d u e -> Vec n (Vec d Bool)
getLastExt (p@PSTransfer{}::PStreamTransfer c n d u e) = getLastExt' @(PStreamTransfer c n d u e) @(HasMultiLast c) p

-- stai
getStai :: PStreamTransfer c n d u e -> StaiType (PStream c n d u e)
getStai PSTransfer{stai} = stai

class StaiDep p b where
  getStaiExt' :: p -> Index (Lanes p)
  mkStai :: Index (Lanes p) -> StaiType p
instance (HasStai c ~ False, KnownNat n) => StaiDep (PStreamTransfer c n d u e) False where
  getStaiExt' _ = 0
  mkStai _ = ()
instance (HasStai c ~ True) => StaiDep (PStreamTransfer c n d u e) True where
  getStaiExt' PSTransfer{stai} = stai
  mkStai = id

getStaiExt :: PStreamTransfer c n d u e -> Index n
getStaiExt (p@PSTransfer{}::PStreamTransfer c n d u e) = getStaiExt' @(PStreamTransfer c n d u e) @(HasStai c) p

-- endi
getEndi :: PStreamTransfer c n d u e -> Index n
getEndi PSTransfer{endi} = endi

-- strb
getStrbRaw :: PStreamTransfer c n d u e -> StrbType (PStream c n d u e)
getStrbRaw PSTransfer{strb} = strb
getStrbExt :: PStreamTransfer c n d u e -> Vec n Bool
getStrbExt p@PSTransfer{} = maskRange (getStaiExt p) (getEndi p) (getStrbExtRaw p)

class StrbDep p b where
  getStrb' :: p -> StrbType p
  getStrbExtRaw' :: p -> Vec (Lanes p) Bool
  mkStrb :: StrbType p -> Vec (Lanes p) Bool -> StrbType p
instance (HasMultiStrb c ~ False) => StrbDep (PStreamTransfer c n d u e) False where
  getStrb' = getStrbRaw
  getStrbExtRaw' PSTransfer{strb} = repeat strb
  mkStrb raw _ext = raw
instance (HasMultiStrb c ~ True) => StrbDep (PStreamTransfer c n d u e) True where
  getStrb' = getStrbExt
  getStrbExtRaw' = getStrbRaw
  mkStrb _raw ext = ext

getStrb :: PStreamTransfer c n d u e -> StrbType (PStreamTransfer c n d u e)
getStrb (p@PSTransfer{}::PStreamTransfer c n d u e) = getStrb' @(PStreamTransfer c n d u e) @(HasMultiStrb c) p
getStrbExtRaw :: PStreamTransfer c n d u e -> Vec n Bool
getStrbExtRaw (p@PSTransfer{}::PStreamTransfer c n d u e) = getStrbExtRaw' @(PStreamTransfer c n d u e) @(HasMultiStrb c) p

-- SETTERS

transfer :: PStreamTransfer c n d u e -> PStream c n d u e
transfer PSTransfer{dat,user,last,stai,endi,strb} = PStream{valid=True,dat,user,last,stai,endi,strb}

class NoTransfer a where
  noTransfer :: a
instance (CompleteComplexity c n d u e) => NoTransfer (PStream c n d u e) where
  noTransfer = PStream{
      valid = False
    , dat = repeat undefined
    , user = undefined
    , last = mkLast @(PStreamTransfer c n d u e) @(HasMultiLast c) (repeat $ repeat undefined :: Vec n (Vec d Bool))
    , stai = mkStai @(PStreamTransfer c n d u e) @(HasStai c) undefined
    , endi = undefined
    , strb = mkStrb @(PStreamTransfer c n d u e) @(HasMultiStrb c) undefined (repeat undefined :: Vec n Bool)
  } :: PStream c n d u e

fromTransfer :: (CompleteComplexity c n d u e) => Maybe (PStreamTransfer c n d u e) -> PStream c n d u e
fromTransfer Nothing = noTransfer
fromTransfer (Just t) = transfer t


fromSignals
  :: (CompleteComplexity c n d u e)
  => Vec n e                      -- ^ data
  -> LastType (PStream c n d u e) -- ^ last
  -> u                            -- ^ user
  -> StaiType (PStream c n d u e) -- ^ stai
  -> Index n                      -- ^ endi
  -> StrbType (PStream c n d u e) -- ^ strb
  -> PStreamTransfer c n d u e
fromSignals dat last user stai endi strb = sealTransfer $ PSTransfer{dat,last,user,stai,endi,strb}

fromStrobed
  :: ( CompleteComplexity c n d u e
     , HasMultiStrb c ~ True
     , HasStai c ~ True -- always true if (HasMultiStrb c ~ True)
     , n~n0+1 )
  => Vec n (Maybe e)
  -> LastType (PStream c n d u e)
  -> u
  -> PStreamTransfer c n d u e
fromStrobed strobedData last user = fromSignals dat last user 0 maxBound strb
  where dat  = fromMaybe undefined <$> strobedData
        strb = isJust <$> strobedData

-- fromSlice :: ...
class FromSlice a bstai bstrb where
  fromSlice :: SliceStrbType a -> LastType a -> UserType a -> a
instance (CompleteComplexity c n d u e, HasStai c ~ False, HasMultiStrb c ~ False)
  => FromSlice (PStreamTransfer c n d u e) False False where -- Maybe Prefix
  fromSlice s last user = fromSignals dat last user () endi strb
    where dat  = maybe (repeat undefined) Prefix.unsafeFromPrefix s
          endi = maybe maxBound Prefix.end s
          strb = isJust s
instance (CompleteComplexity c n d u e, HasStai c ~ True, HasMultiStrb c ~ False)
  => FromSlice (PStreamTransfer c n d u e) True False where -- Maybe Slice
  fromSlice s last user = fromSignals dat last user stai endi strb
    where dat  = maybe (repeat undefined) Slice.unsafeFromSlice s
          stai = maybe 0        Slice.start s
          endi = maybe maxBound Slice.end   s
          strb = isJust s
instance (CompleteComplexity c n d u e, HasStai c ~ True, HasMultiStrb c ~ True)
  => FromSlice (PStreamTransfer c n d u e) True True where -- Slice Maybe
  fromSlice s last user = fromSignals dat last user stai endi strb
    where dat  = map fromJust $ Slice.unsafeFromSlice s
          stai = Slice.start s
          endi = Slice.end   s
          strb = map isJust $ Slice.unsafeFromSlice s


-- SEAL

seal :: PStream c n d u e -> PStream c n d u e
seal p@PStream{} = fromTransfer $ sealTransfer <$> getTransfer p

class SealTransfer a where
  sealTransfer :: a -> a
instance SealTransfer (PStreamTransfer c n d u e) where
  sealTransfer p@PSTransfer{dat,user,last,stai,endi,strb} = PSTransfer{
      dat  = zipWith undefFromBool (getStrbExt p) dat
    , user = user
    , last = last
    , stai = stai
    , endi = endi
    , strb = mkStrb @(PStreamTransfer c n d u e) @(HasMultiStrb c)
                strb
                (zipWith
                  undefFromBool
                  (maskRange (getStaiExt p) endi (repeat True))
                  (getStrbExtRaw p))
  } :: PStreamTransfer c n d u e


-- HELPER

maskRange :: (KnownNat n) => Index n -> Index n -> Vec n Bool -> Vec n Bool
maskRange s e = zipWith (\i b -> s<=i && i<=e && b) indicesI

fromBool :: Bool -> a -> Maybe a
fromBool False _ = Nothing
fromBool True  x = Just x

undefFromBool :: Bool -> a -> a
undefFromBool False _ = undefined
undefFromBool True  x = x

-- STANDARD CLASSES (Maybe behaviour)

-- functor: not possible :(
-- instance Functor (PStream c n d u e) where
--     fmap :: (a -> b) -> f a -> f b

tfmap -- map function to a stream's transfers
  :: (CompleteComplexity c' n' d' u' e')
  => (PStreamTransfer c n d u e -> PStreamTransfer c' n' d' u' e')
  -> PStream c n d u e
  -> PStream c' n' d' u' e'
tfmap f p = fromTransfer $ f <$> getTransfer p

infixl 4 <$$>
(<$$>)
  :: (CompleteComplexity c' n' d' u' e')
  => (PStreamTransfer c n d u e -> PStreamTransfer c' n' d' u' e')
  -> PStream c n d u e
  -> PStream c' n' d' u' e'
(<$$>) = tfmap

-- eq
instance (CompleteComplexity c n d u e, Eq u, Eq e) => Eq (PStream c n d u e) where
  (==) a b = getTransfer a == getTransfer b

instance (CompleteComplexity c n d u e, Eq u, Eq e) => Eq (PStreamTransfer c n d u e) where
  (==) a b =
       (getStaiExt a == getStaiExt b)
    && (getEndi a == getEndi b)
    && (getUser a == getUser b)
    && (getDataStrobed a == getDataStrobed b)
    && (getLastExt a == getLastExt b)


-- monad
-- not possible; see the functor reason. Would require rewriting `PStream c n d u e` to `PStream (PStreamTransfer c n d u e)`
-- Type PStream c n d u e = PStream' (PSTransfer c n d u e)


-- OPTICS



-- show

instance (Show (PStreamTransfer c n d u e)) => Show (PStream c n d u e) where
  show p = case getTransfer p of
    Just t -> "Transfer (" <> show t <> ")"
    Nothing -> "NoTransfer"
instance
  ( Show u
  , Show e
  , n~n0+1
  , Show (LastType (PStream c n d u e))
  , Show (StaiType (PStream c n d u e))
  )
  => Show (PStreamTransfer c n d u e) where
  show p@PSTransfer{} = "PStreamTransfer" <> dats <> show (getLast p) <> show (getUser p)
    where dats = "[" <> strobeds <> "][" <> show (getStai p) <> " ..= " <> show (getEndi p) <> "]"
          strobeds = foldl1 (\a b -> a <> "," <> b) $ map disp $ getDataStrobed p
          disp (Just x) = show x
          disp Nothing = "-"


-- shockwaves
