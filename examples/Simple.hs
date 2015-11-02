module Simple where



import Prelude ()

import Feldspar
import qualified Feldspar.Mutable as Feld
import Feldspar.SimpleVector
import Feldspar.IO
import Feldspar.IO.Mutable



kernel :: Data Int32 -> Data Int32
kernel n = sum $ map (\x -> x*x) (0...n)

prog = while (return true) $ do
    i   <- fget stdin
    end <- feof stdin
    iff end break (return ())
    o <- ifE (i > 0)
        (return $ kernel (guaranteePositive i))
        (return 0)
    printf "Sum of squares: %d\n" o
  where
    guaranteePositive = cap (Range 1 maxBound)

arrProg = do
    lr <- initRef 20
    l  <- unsafeFreezeRef lr
    arr1 :: Arr WordN WordN <- unsafeThawArr $ parallel l (*2)
    e <- getArr 18 arr1
    printf "%d\n" e
    arr2 :: Arr WordN Int8 <- newArr 123
    setArr 23 88 arr2
    farr <- freezeArr arr2 123
    printf "%d\n" (farr ! 23)

mut :: Data WordN -> Feld.M (Data WordN)
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

