{-# LANGUAGE CPP #-}

module Feldspar.IO.Frontend
  ( module Feldspar.IO.Frontend
  , ExternalCompilerOpts (..)
  , defaultExtCompilerOpts
  ) where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Ix
import Data.Monoid
import Data.Proxy
import Text.Printf (PrintfArg)

import qualified Control.Monad.Operational.Higher as Imp
import Language.Embedded.Imperative.CMD (FileCMD (..))
import Language.Embedded.Imperative.Frontend.General
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp
import Language.Embedded.Backend.C (ExternalCompilerOpts (..), defaultExtCompilerOpts)
import qualified Language.Embedded.Backend.C as Imp

import Feldspar (Type, Data, WordN (..))
import Feldspar.Compiler.FromImperative (feldsparCIncludes)
import Feldspar.IO.CMD



deriving instance PrintfArg WordN    -- TODO Should go into feldspar-language
deriving instance Read WordN         -- TODO Should go into feldspar-language
deriving instance Formattable WordN  -- TODO Should go into feldspar-compiler-shim



-- | Program monad
newtype Program a = Program {unProgram :: Imp.Program FeldCMD a}
  deriving (Functor, Applicative, Monad)



--------------------------------------------------------------------------------
-- * References
--------------------------------------------------------------------------------

-- | Create an uninitialized reference
newRef :: Type a => Program (Ref a)
newRef = Program Imp.newRef

-- | Create an initialized reference
initRef :: Type a => Data a -> Program (Ref a)
initRef = Program . Imp.initRef

-- | Get the contents of a reference
getRef :: Type a => Ref a -> Program (Data a)
getRef = Program . Imp.getRef

-- | Set the contents of a reference
setRef :: Type a => Ref a -> Data a -> Program ()
setRef r = Program . Imp.setRef r

-- | Modify the contents of reference
modifyRef :: Type a => Ref a -> (Data a -> Data a) -> Program ()
modifyRef r f = Program $ Imp.modifyRef r f

-- | Freeze the contents of reference (only safe if the reference is not updated
-- as long as the resulting value is alive)
unsafeFreezeRef :: Type a => Ref a -> Program (Data a)
unsafeFreezeRef = Program . Imp.unsafeFreezeRef

-- | Compute and share a value. Like 'share' but using the 'Program' monad
-- instead of a higher-order interface.
shareVal :: Type a => Data a -> Program (Data a)
shareVal a = initRef a >>= unsafeFreezeRef



--------------------------------------------------------------------------------
-- * Arrays
--------------------------------------------------------------------------------

-- | Create an uninitialized array
newArr :: (Type a, Type i, Integral i, Ix i) => Data i -> Program (Arr i a)
newArr n = Program $ Imp.newArr n

-- | Create an uninitialized array of unknown size
newArr_ :: (Type a, Type i, Integral i, Ix i) => Program (Arr i a)
newArr_ = Program $ Imp.newArr_

-- | Get an element of an array
getArr :: (Type a, Type i, Integral i, Ix i) =>
    Data i -> Arr i a -> Program (Data a)
getArr i arr = Program $ Imp.getArr i arr

-- | Set an element of an array
setArr :: (Type a, Type i, Integral i, Ix i) =>
    Data i -> Data a -> Arr i a -> Program ()
setArr i a arr = Program $ Imp.setArr i a arr

-- | Copy the contents of an array to another array. The number of elements to
-- copy must not be greater than the number of allocated elements in either
-- array.
copyArr :: (Type a, Type i, Integral i, Ix i)
    => Arr i a  -- ^ Destination
    -> Arr i a  -- ^ Source
    -> Data i   -- ^ Number of elements
    -> Program ()
copyArr arr1 arr2 len = Program $ Imp.copyArr arr1 arr2 len

thawArr :: (Type a, Num n, Ix n) => Data [a] -> Program (Arr n a)
thawArr = Program . Imp.singleInj . ThawArr

unsafeThawArr :: (Type a, Num n, Ix n) => Data [a] -> Program (Arr n a)
unsafeThawArr = Program . Imp.singleInj . UnsafeThawArr

freezeArr :: (Type a, Num n, Ix n) => Arr n a -> Data n -> Program (Data [a])
freezeArr a n = Program $ Imp.singleInj $ FreezeArr a n



--------------------------------------------------------------------------------
-- * Control flow
--------------------------------------------------------------------------------

-- | Conditional statement
iff
    :: Data Bool   -- ^ Condition
    -> Program ()  -- ^ True branch
    -> Program ()  -- ^ False branch
    -> Program ()
iff b t f = Program $ Imp.iff b (unProgram t) (unProgram f)

-- | Conditional statement that returns an expression
ifE :: Type a
    => Data Bool         -- ^ Condition
    -> Program (Data a)  -- ^ True branch
    -> Program (Data a)  -- ^ False branch
    -> Program (Data a)
ifE b t f = Program $ Imp.ifE b (unProgram t) (unProgram f)

-- | For loop
for :: (Integral n, Type n)
    => IxRange (Data n)        -- ^ Index range
    -> (Data n -> Program ())  -- ^ Loop body
    -> Program ()
for range body = Program $ Imp.for range (unProgram . body)

-- | While loop
while
    :: Program (Data Bool)  -- ^ Continue condition
    -> Program ()           -- ^ Loop body
    -> Program ()
while b t = Program $ Imp.while (unProgram b) (unProgram t)

-- | Assertion
assert
    :: Data Bool  -- ^ Expression that should be true
    -> String     -- ^ Message in case of failure
    -> Program ()
assert cond msg = Program $ Imp.assert cond msg

-- | Break out from a loop
break :: Program ()
break = Program Imp.break



--------------------------------------------------------------------------------
-- * File handling
--------------------------------------------------------------------------------

-- | Open a file
fopen :: FilePath -> IOMode -> Program Handle
fopen file = Program . Imp.fopen file

-- | Close a file
fclose :: Handle -> Program ()
fclose = Program . Imp.fclose

-- | Check for end of file
feof :: Handle -> Program (Data Bool)
feof = Program . Imp.feof

class PrintfType r
  where
    fprf :: Handle -> String -> [Imp.PrintfArg Data] -> r

instance (a ~ ()) => PrintfType (Program a)
  where
    fprf h form as = Program $ Imp.singleE $ FPrintf h form (reverse as)

instance (Formattable a, Type a, PrintfType r) => PrintfType (Data a -> r)
  where
    fprf h form as = \a -> fprf h form (Imp.PrintfArg a : as)

-- | Print to a handle. Accepts a variable number of arguments.
fprintf :: PrintfType r => Handle -> String -> r
fprintf h format = fprf h format []

-- | Put a single value to a handle
fput :: (Formattable a, Type a)
    => Handle
    -> String  -- Prefix
    -> Data a  -- Expression to print
    -> String  -- Suffix
    -> Program ()
fput h pre a post = Program $ Imp.fput h pre a post

-- | Get a single value from a handle
fget :: (Formattable a, Type a) => Handle -> Program (Data a)
fget = Program . Imp.fget

-- | Print to @stdout@. Accepts a variable number of arguments.
printf :: PrintfType r => String -> r
printf = fprintf Imp.stdout



--------------------------------------------------------------------------------
-- * Abstract objects
--------------------------------------------------------------------------------

newObject
    :: String  -- ^ Object type
    -> Program Object
newObject = Program . Imp.newObject

initObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Program Object
initObject fun ty args = Program $ Imp.initObject fun ty args

initUObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Program Object
initUObject fun ty args = Program $ Imp.initUObject fun ty args



--------------------------------------------------------------------------------
-- * External function calls (C-specific)
--------------------------------------------------------------------------------

-- | Add an @#include@ statement to the generated code
addInclude :: String -> Program ()
addInclude = Program . Imp.addInclude

-- | Add a global definition to the generated code
--
-- Can be used conveniently as follows:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- >
-- > import Feldspar.IO
-- >
-- > prog = do
-- >     ...
-- >     addDefinition myCFunction
-- >     ...
-- >   where
-- >     myCFunction = [cedecl|
-- >       void my_C_function( ... )
-- >       {
-- >           // C code
-- >           // goes here
-- >       }
-- >       |]
addDefinition :: Definition -> Program ()
addDefinition = Program . Imp.addDefinition

-- | Declare an external function
addExternFun :: forall proxy res . Type res
    => String         -- ^ Function name
    -> proxy res      -- ^ Proxy for expression and result type
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Program ()
addExternFun fun res args = Program $ Imp.addExternFun fun res' args
  where
    res' = Proxy :: Proxy (Data res)

-- | Declare an external procedure
addExternProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Program ()
addExternProc proc args = Program $ Imp.addExternProc proc args

-- | Call a function
callFun :: Type a
    => String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Program (Data a)
callFun fun as = Program $ Imp.callFun fun as

-- | Call a procedure
callProc
    :: String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Program ()
callProc fun as = Program $ Imp.callProc fun as

-- | Declare and call an external function
externFun :: Type res
    => String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Program (Data res)
externFun fun args = Program $ Imp.externFun fun args

-- | Declare and call an external procedure
externProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Program ()
externProc proc args = Program $ Imp.externProc proc args

-- | Get current time as number of seconds passed today
getTime :: Program (Data Double)
getTime = Program Imp.getTime

strArg :: String -> FunArg Data
strArg = Imp.strArg

valArg :: Type a => Data a -> FunArg Data
valArg = Imp.valArg

refArg :: Type a => Ref a -> FunArg Data
refArg = Imp.refArg

arrArg :: Type a => Arr n a -> FunArg Data
arrArg = Imp.arrArg

objArg :: Object -> FunArg Data
objArg = Imp.objArg

addr :: FunArg Data -> FunArg Data
addr = Imp.addr



--------------------------------------------------------------------------------
-- * Back ends
--------------------------------------------------------------------------------

-- | Interpret a program in the 'IO' monad
runIO :: Program a -> IO a
runIO = Imp.runIO . unProgram

-- | Like 'runIO' but with explicit input/output connected to @stdin@/@stdout@
captureIO
    :: Program a  -- ^ Program to run
    -> String     -- ^ Faked @stdin@
    -> IO String  -- ^ Captured @stdout@
captureIO = Imp.captureIO . unProgram

-- | Compile a program to C code represented as a string. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C YOURPROGRAM.c
--
-- For programs that make use of the primitives in "Feldspar.Concurrent", some
-- extra flags are needed:
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C -Ipath/to/imperative-edsl/include path/to/imperative-edsl/csrc/chan.c -lpthread YOURPROGRAM.c
compile :: Program a -> String
compile = Imp.compile . unProgram

-- | Compile a program to C code and print it on the screen. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C YOURPROGRAM.c
--
-- For programs that make use of the primitives in "Feldspar.Concurrent", some
-- extra flags are needed:
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C -Ipath/to/imperative-edsl/include path/to/imperative-edsl/csrc/chan.c -lpthread YOURPROGRAM.c
icompile :: Program a -> IO ()
icompile = putStrLn . compile

addFeldsparCIncludes :: ExternalCompilerOpts -> IO ExternalCompilerOpts
addFeldsparCIncludes opts = do
    feldLib <- feldsparCIncludes
    return $ opts <> mempty { externalFlagsPre = ["-I" ++ feldLib] }

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck' :: ExternalCompilerOpts -> Program a -> IO ()
compileAndCheck' opts prog = do
    opts' <- addFeldsparCIncludes opts
    Imp.compileAndCheck' opts' (unProgram prog)

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck :: Program a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled' :: ExternalCompilerOpts -> Program a -> IO ()
runCompiled' opts prog = do
    opts' <- addFeldsparCIncludes opts
    Imp.runCompiled' opts' (unProgram prog)

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled :: Program a -> IO ()
runCompiled = runCompiled' mempty

-- | Like 'runCompiled'' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled'
    :: ExternalCompilerOpts
    -> Program a  -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
captureCompiled' opts prog inp = do
    opts' <- addFeldsparCIncludes opts
    Imp.captureCompiled' opts' (unProgram prog) inp

-- | Like 'runCompiled' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled
    :: Program a  -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
captureCompiled = captureCompiled' mempty

-- | Compare the content written to 'stdout' from interpretation in 'IO' and
-- from running the compiled C code
compareCompiled'
    :: ExternalCompilerOpts
    -> Program a  -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled' opts prog inp = do
    opts' <- addFeldsparCIncludes opts
    Imp.compareCompiled' opts' (unProgram prog) inp

-- | Compare the content written to 'stdout' from interpretation in 'IO' and
-- from running the compiled C code
compareCompiled
    :: Program a  -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled = compareCompiled' mempty

