
module Tydi.PStream (
  -- PStream
  PStream, PStreamTransfer, PStreamReady, Complexity(..),

  -- Reading
  getTransfer,isValid,


  -- Writing
  fromTransfer,fromTransferM,

  -- Sealing
  seal,

) where

import Tydi.Internal.PStream
import Tydi.Internal.PStreamRead
import Tydi.Internal.PStreamWrite
import Tydi.Internal.PStreamSeal
