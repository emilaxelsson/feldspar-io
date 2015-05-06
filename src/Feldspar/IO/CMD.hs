{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Feldspar.IO.CMD where



import Data.Array
import Data.Array.IO
import Data.List (genericLength)
import Data.Typeable

import Language.C.Quote.C
import Text.PrettyPrint.Mainland

import Control.Monad.Operational.Compositional
import Language.C.Monad
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Concurrent

import Feldspar
import Feldspar.Compiler.FromImperative ()



pData :: Proxy Data
pData = Proxy

type FeldCMD
    =   RefCMD     Data
    :+: ArrCMD     Data
    :+: ControlCMD Data
    :+: FileCMD    Data
    :+: CallCMD    Data
    :+: ChanCMD    Data
    :+: ThreadCMD
    :+: ArrConvCMD

data ArrConvCMD (prog :: * -> *) a
  where
    ThawArr       :: (Type a, Num n, Ix n) => Data [a] -> ArrConvCMD prog (Arr n a)
    UnsafeThawArr :: (Type a, Num n, Ix n) => Data [a] -> ArrConvCMD prog (Arr n a)
    FreezeArr :: (Type a, Ix n)        => Arr n a -> ArrConvCMD prog (Data [a])
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

instance MapInstr ArrConvCMD
  where
    imap _ (ThawArr arr)       = ThawArr arr
    imap _ (UnsafeThawArr arr) = UnsafeThawArr arr
    imap _ (FreezeArr arr)     = FreezeArr arr

instance DryInterp ArrConvCMD
  where
    dryInterp (ThawArr _)       = liftM ArrComp $ freshStr "a"
    dryInterp (UnsafeThawArr _) = liftM ArrComp $ freshStr "a"
    dryInterp (FreezeArr _)     = liftM varExp fresh

type instance IExp ArrConvCMD         = Data
type instance IExp (ArrConvCMD :+: i) = Data

runArrConvCMD :: ArrConvCMD prog a -> IO a
runArrConvCMD (ThawArr arr) = fmap ArrEval $ thaw' $ evalExp arr
  where
    thaw' as = newListArray (0, genericLength as - 1) as
runArrConvCMD (UnsafeThawArr arr) = runArrConvCMD (ThawArr arr)
runArrConvCMD (FreezeArr (ArrEval arr)) = fmap litExp $ freeze' arr
  where
    freeze' arr = fmap elems $ freeze arr

compArrConvCMD :: ArrConvCMD prog a -> CGen a
compArrConvCMD (ThawArr arr) = do
    arre <- compExp arr
    sym  <- gensym "a"
    t    <- compTypePP2 pData arr
    let tsym = show $ ppr t
        lene = [cexp| getLength( $arre ) |]
    addLocal [cdecl| $ty:t * $id:sym; |]
    addStm   [cstm| $id:sym = calloc( $lene, sizeof($id:tsym) ); |]
    addStm   [cstm| memcpy( $id:sym, &at($id:tsym, $arre, 0), $lene * sizeof($id:tsym) ); |]
    return $ ArrComp sym
compArrConvCMD (UnsafeThawArr arr) = do
    arre <- compExp arr
    sym  <- gensym "a"
    t    <- compTypePP2 pData arr
    let tsym = show $ ppr t
    addLocal [cdecl| $ty:t * $id:sym; |]
    addStm   [cstm| $id:sym = &at($id:tsym, $arre, 0); |]
    return $ ArrComp sym
compArrConvCMD (FreezeArr (ArrComp arr)) = do
    (v,n) <- freshVar
--     addLocal [cdecl| struct array * $id:n = NULL; |]
    return v

instance Interp ArrConvCMD IO   where interp = runArrConvCMD
instance Interp ArrConvCMD CGen where interp = compArrConvCMD

