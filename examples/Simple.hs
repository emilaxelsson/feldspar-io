module Simple where



import Prelude ()

import Feldspar hiding (getRef, setRef, newArr, getArr, setArr)
import Feldspar.SimpleVector
import Feldspar.IO



kernel :: Data Int32 -> Data Int32
kernel n = sum $ map (\x -> x*x) (0...n)

prog = while (return true) $ do
    i <- fget stdin
    o <- ifE (i > 0)
        (return $ kernel (cap (Range 1 maxBound) i))
        (return 0)
    printf "Sum of squares: %d\n" o

arrProg = do
    lr  <- initRef 20
    arr <- unsafeThawArr $ parallel (unsafeFreezeRef lr) (*2)
    e   <- getArr 18 (arr :: Arr WordN WordN)
    printf "%d\n" e
    arr  <- newArr 123
    setArr 23 (88 :: Data Int8) arr
    arr' <- freezeArr (arr :: Arr WordN Int8)
    printf "%d\n" (arr' ! 23)

