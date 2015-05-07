module Simple where



import Prelude ()

import Feldspar hiding (newRef, getRef, setRef, newArr, getArr, setArr)
import qualified Feldspar as Feld
import Feldspar.SimpleVector
import Feldspar.IO



kernel :: Data Int32 -> Data Int32
kernel n = sum $ map (\x -> x*x) (0...n)

prog = while (return true) $ do
    i   <- fget stdin
    end <- feof stdin
    iff end break (return ())
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

mut :: Data WordN -> M (Data WordN)
mut l = do
    arr <- Feld.newArr_ l
    Feld.forM l $ \i -> do
        Feld.setArr arr i i
    Feld.getArr arr 34

mutProg :: Program ()
mutProg = do
    r <- initRef 100
    l <- getRef r
    printf "%d\n" =<< liftMut (mut l)

