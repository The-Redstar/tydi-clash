

import Tydi
import Tydi.Internal.PStream
import Tydi.LStream



type Interface = New C8 1 () (Unsigned 32)

type Fd = TydiSynth Interface
type Bd = Reverse Fd

type P = PStream C8 4 0 () (Unsigned 32) 'True
