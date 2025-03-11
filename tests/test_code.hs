

import Tydi
import Tydi.Internal.PStream
import Tydi.LStream



type Interface = New C8 1 () (Unsigned 32)

type Fd = TydiSynth Interface
type Bd = Reverse Fd

type P = PStream C8 4 0 () (Unsigned 32) 'True



ghci> type Interface = $(synthBundle $ LStream 0 Desync Forward False 7 4 [t|Bool|] (Group (L @"x" (d [t|Int|]) :*: L @"y" (d [t|Int|]))))
ghci> :i Interface
type Interface :: ghc-prim-0.10.0:GHC.Types.Type
type Interface =
  StreamNode
    (PStream C8 4 0 Bool (Group (L "x" Int :*: L "y" Int)) True)
    (Group (L "x" () :*: L "y" ()))
        -- Defined at <interactive>:12:1
ghci>


Group (L @"x" (d [t|Int|]) :*: L @"y" (d [t|Int|]))
group [
  "x" >:: d[t|Int|],
  "y" >:: d[t|Int|],
  "q" >:: substream .... . .. $ group [
    ...
  ]
]
