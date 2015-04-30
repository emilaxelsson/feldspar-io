module Simple where



import Prelude ()

import Feldspar hiding (getRef, setRef)
import Feldspar.SimpleVector
import Feldspar.IO



kernel :: Data Int32 -> Data Int32
kernel n = sum $ map (\x -> x*x) (0...n)

prog = do
    r <- initRef 100
    i <- getRef r
    setRef r $ kernel (cap (Range 1 maxBound) i)
    o <- getRef r
    printf "%d" o

