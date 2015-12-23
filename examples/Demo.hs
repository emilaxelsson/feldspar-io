{-# LANGUAGE QuasiQuotes #-}

module Demo where



import qualified Prelude
import Control.Applicative ((<$>))

import Feldspar
import Feldspar.IO



------------------------------------------------------------

sumInput :: Program ()
sumInput = do
    done <- initRef false
    sum  <- initRef (0 :: Data Word32)
    while (not <$> getRef done) $ do
        printf "Enter a number (0 means done): "
        n <- fget stdin
        iff (n == 0)
          (setRef done true)
          (modifyRef sum (+n))
--     abort
--     printSum sum
    printf "The sum of your numbers is %d.\n" =<< getRef sum

abort :: Program ()
abort = do
    addInclude "<stdlib.h>"
    callProc "abort" []

printSum :: Ref Word32 -> Program ()
printSum s = do
    addDefinition printSum_def
    callProc "printSum" [refArg s]

printSum_def = [cedecl|
    void printSum (typename uint32_t * s) {
        printf ("I think the sum of your numbers is %d.\n", *s);
    }
    |]



------------------------------------------------------------

fib :: Data Word32 -> Data Word32
fib n = fst $ forLoop (i2n n) (0,1) $ \_ (a,b) -> (b,a+b)

printFib :: Program ()
printFib = do
    printf "Enter a positive number: "
    n <- fget stdin
    printf "The %dth Fibonacci number is %d.\n" n (fib n)



------------------------------------------------------------

testAll = do
    compileAndCheck sumInput
    compileAndCheck printFib

