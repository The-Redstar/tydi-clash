
-- american english is ugly :(
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Tydi.Behavioral where

import Clash.Prelude
import Tydi.PStream
import Tydi.Synthesis (Reverse)
import Data.Proxy (Proxy(..))

{-

general:
  values cannot change while valid is high, unless ready was high

  if (previous transfer) and (not previous ready) and current != previous then problem



complexities

C1 Whole outermost instances must be transferred in consecutive cycles.
- state: boolean to see if data was previously being transmitted
- if it was, valid cannot go low until the first bit of LAST is high

C2 Innermost sequences must be transferred in consecutive cycles.
- see C1, but last LAST bit

C3 The last flag cannot be postponed until after the transfer of the last element.
- state: see C4, but for every dimension
- for every dimension:
    if (state) and (no current data) and (bit in LAST) then PROBLEM

C4 All lanes must be active for all but the last transfer of the innermost sequence.
- state: boolean to see if data was previously being transmitted
  - next state = (not last of LAST) && (some data transmitted)
- if (state) and (not last of LAST) and (not all lanes active) then PROBLEM


complexity 5+ -> only valid change check
dim 0 -> only valid change check

-}

type family RequiresChecks p where
  RequiresChecks (PStream c     n 0 u e) = False -- all behaviour checks are for checking sequences
  RequiresChecks (PStream (C 1) n d u e) = True
  RequiresChecks (PStream (C 2) n d u e) = True
  RequiresChecks (PStream (C 3) n d u e) = True
  RequiresChecks (PStream (C 4) n d u e) = True
  RequiresChecks _                       = False

class (RequiresChecks p ~ complex) => Check p (complex::Bool) where
  check :: (HiddenClockResetEnable dom) => (Signal dom p -> Signal dom (Reverse p) -> (Signal dom p, Signal dom (Reverse p)))

instance (
    CompleteComplexity' c n d u e
  , NFDataX (PStream c n d u e)
  , Eq u
  , Eq e
  , RequiresChecks (PStream c n d u e) ~ False
  ) => Check (PStream c n d u e) False where
  check = checkValid
instance (
    HasStai      c ~ False
  , HasMultiLast c ~ False
  , HasMultiStrb c ~ False
  , CompleteComplexity' c n d u e
  , Eq (PStream c n d u e)
  , NFDataX (PStream c n d u e)
  , d~d0+1
  -- , n~n0+1
  , RequiresChecks (PStream c n d u e) ~ True
  , c ~ C c0
  , KnownNat c0
  ) => Check (PStream c n d u e) True where
  check p r = checkBehav p' r' --curry checkBehav $ checkValid p r
    where (p',r') = checkValid p r

-- assert that valid does not go low, and the data does not change, until the transfer has been accepted by the sink
checkValid :: (
    CompleteComplexity' c n d u e
    , Eq (PStream c n d u e)
    , NFDataX (PStream c n d u e)
    , HiddenClockResetEnable dom
  ) => Signal dom (PStream c n d u e) -> Signal dom (PStreamReady c n d u e) -> (Signal dom (PStream c n d u e), Signal dom (PStreamReady c n d u e))
checkValid pstream ready = unbundle $ mealy go init' (bundle (pstream,ready))
  where
    init' = (NoTransfer, NotReady)
    go :: (Eq (PStream c n d u e))
       => (PStream c n d u e, PStreamReady c n d u e)
       -> (PStream c n d u e, PStreamReady c n d u e)
       -> ((PStream c n d u e, PStreamReady c n d u e), (PStream c n d u e, PStreamReady c n d u e))
    go (p',r') (p,r) = ((p,r), o)
      where o = if isValid p' && (r'==NotReady) && (p' /= p) then
                  (err,err)
                else
                  (p,r)
            err = errorX "Stream value changed after setting valid"

-- assert behavioural checks are in order
checkBehav :: forall c n d u e dom d0. (
    HiddenClockResetEnable dom
  , HasStai      (C c) ~ False
  , HasMultiLast (C c) ~ False
  , HasMultiStrb (C c) ~ False
  , KnownNat d
  , KnownNat c
  , KnownNat n
  , d~d0+1
  ) => Signal dom (PStream (C c) n d u e) -> Signal dom (PStreamReady (C c) n d u e) -> (Signal dom (PStream (C c) n d u e), Signal dom (PStreamReady (C c) n d u e))
checkBehav pstream ready = unbundle $ mealy go init' (bundle (pstream,ready))
  where
    init' = (repeat False,False) -- state indicates whether data has been sent previously without being terminated, per dimension level
    go (prevUnterminatedData,prevUnfullData) (p,r) = ((nextUnterminatedData,nextUnfullData), o) -- previous transfer, not cycle
      where
        -- next cycle, prev transfer
        (nextUnterminatedData,nextUnfullData) = case (p,r) of
          (Transfer tf,Ready) -> (
                zipWith3 (\prev cur term -> (prev || cur) && not term) prevUnterminatedData dataPresent (getLast tf)
              , unfullData && not (last $ getLast tf)
            )
          _                   -> (prevUnterminatedData,prevUnfullData)

        -- there is a transfer with not all data lanes (but some?) active
        unfullData = case p of
          Transfer tf -> (getEndi tf /= maxBound) && getStrb tf -- or || not (getStrb tf)
          _           -> False

        last' = maybe (repeat False) getLast $ getTransfer p

        -- data present per complexity level
        dataPresent = case p of
          Transfer tf -> tail $ scanr (||) (getStrb tf) $ getLast tf --postfix OR of `last`, starting with there being data for the innermost sequence
          _           -> repeat False

        -- nextstate = zipWith (\a b -> a && (! b)) (repeat dataPresent) (last')
        c = natVal $ Proxy @c

        -- C1: if not dataPresent && any of state, PROBLEM -- unless a higher bit of LAST is high while its state is low, indicating an empty sequence
        (p1,r1) = if c<=1 then (p ,r ) else
          if or $ zipWith (\d ut -> not d && ut) dataPresent prevUnterminatedData then
            (err1,err1)
          else (p, r)
        -- C2: if not dataPresent && last state, PROBLEM
        (p2,r2) = if c<=2 then (p1,r1) else
          if not (last dataPresent) && last prevUnterminatedData then
            (err2,err2)
          else (p1, r1)
        -- C3: if the last flag is raised while there is no data, and there was previous data (else this is the valid transfer of an empty sequence), then PROBLEM
        (p3,r3) = if c<=3 then (p2,r2) else
          if or $ zipWith3 (\d ut l -> not d && ut && l) dataPresent prevUnterminatedData last' then
            (err3,err3)
          else (p2, r2)

        -- C4: if (there is a transfer) and (not all data lanes are used), then (this must be the last data transfer cycle),
        -- but the last flag may come in later
        -- so if (previously unterminated non-full transfer) and (data present) then PROBLEM
        (p4,r4) = if c<=4 then (p3,r3) else
          if prevUnfullData && last dataPresent then
            (err4,err4)
          else (p3, r3)

        o = (p4,r4)

        err1 = errorX "..."
        err2 = errorX "..."
        err3 = errorX "Last set in transfer after transfer containing data"
        err4 = errorX "..."

      -- where o = if (isValid p') && (r'==NotReady) && (p' != p) then
      --             (err,err)
      --           else
      --             (p,r)
      --       err = errorX "Stream value changed after setting valid"
