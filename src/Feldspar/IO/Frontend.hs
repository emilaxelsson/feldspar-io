{-# LANGUAGE CPP #-}

module Feldspar.IO.Frontend where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Ix
import Data.Proxy
import Text.Printf (PrintfArg)

import qualified Control.Monad.Operational.Compositional as Imp
import Language.Embedded.Imperative.CMD (FileCMD (..))
import Language.Embedded.Imperative.Frontend.General
import qualified Language.Embedded.Imperative as Imp

import Feldspar (Type, Data, WordN (..))
import Feldspar.IO.CMD



deriving instance PrintfArg WordN    -- TODO Should go into feldspar-language
deriving instance Read WordN         -- TODO Should go into feldspar-language
deriving instance Formattable WordN  -- TODO Should go into feldspar-compiler-shim



-- | Program monad
newtype Program a = Program {unProgram :: Imp.Program FeldCMD a}
  deriving (Functor, Applicative, Monad)



--------------------------------------------------------------------------------
-- * Back ends
--------------------------------------------------------------------------------

-- | Interpret a program in the 'IO' monad
run :: Program a -> IO a
run = Imp.interpret . unProgram

-- | Compile a program to C code represented as a string. To compile the
-- resulting C code, use
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C YOURPROGRAM.c
--
-- For programs that make use of the primitives in "Feldspar.Concurrent", some
-- extra flags are needed:
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C -Ipath/to/imperative-edsl/include path path/to/imperative-edsl/csrc/chan.c -lpthread YOURPROGRAM.c
compile :: Program a -> String
compile = Imp.compile . unProgram

-- | Compile a program to C code and print it on the screen. To compile the
-- resulting C code, use
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C YOURPROGRAM.c
--
-- For programs that make use of the primitives in "Feldspar.Concurrent", some
-- extra flags are needed:
--
-- > gcc -std=c99 -Ipath/to/feldspar-compiler/lib/Feldspar/C -Ipath/to/imperative-edsl/include path path/to/imperative-edsl/csrc/chan.c -lpthread YOURPROGRAM.c
icompile :: Program a -> IO ()
icompile = putStrLn . compile



--------------------------------------------------------------------------------
-- * User interface
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

-- | Freeze the contents of reference (only safe if the reference is never written to after the
-- first action that makes use of the resulting expression)
unsafeFreezeRef :: Type a => Ref a -> Data a
unsafeFreezeRef = Imp.unsafeFreezeRef

-- | Create an uninitialized an array
newArr :: (Type a, Type i, Integral i, Ix i) => Data i -> Program (Arr i a)
newArr n = Program $ Imp.newArr n

-- | Set the contents of an array
getArr :: (Type a, Integral i, Ix i) => Data i -> Arr i a -> Program (Data a)
getArr i arr = Program $ Imp.getArr i arr

-- | Set the contents of an array
setArr :: (Type a, Integral i, Ix i) => Data i -> Data a -> Arr i a -> Program ()
setArr i a arr = Program $ Imp.setArr i a arr

thawArr :: (Type a, Num n, Ix n) => Data [a] -> Program (Arr n a)
thawArr = Program . Imp.singleInj . ThawArr

unsafeThawArr :: (Type a, Num n, Ix n) => Data [a] -> Program (Arr n a)
unsafeThawArr = Program . Imp.singleInj . UnsafeThawArr

freezeArr :: (Type a, Num n, Ix n) => Arr n a -> Data n -> Program (Data [a])
freezeArr a n = Program $ Imp.singleInj $ FreezeArr a n

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

-- | While loop
while
    :: Program (Data Bool)  -- ^ Continue condition
    -> Program ()           -- ^ Loop body
    -> Program ()
while b t = Program $ Imp.while (unProgram b) (unProgram t)

-- | While loop that returns an expression
whileE :: Type a
    => Program (Data Bool)  -- ^ Continue condition
    -> Program (Data a)     -- ^ Loop body
    -> Program (Data a)
whileE b t = Program $ Imp.whileE (unProgram b) (unProgram t)

-- | For loop
for :: (Integral n, Type n)
    => Data n                  -- ^ Start index
    -> Data n                  -- ^ Stop index
    -> (Data n -> Program ())  -- ^ Loop body
    -> Program ()
for lo hi body = Program $ Imp.for lo hi (unProgram . body)

-- | For loop
forE :: (Integral n, Type n, Type a)
    => Data n                        -- ^ Start index
    -> Data n                        -- ^ Stop index
    -> (Data n -> Program (Data a))  -- ^ Loop body
    -> Program (Data a)
forE lo hi body = Program $ Imp.forE lo hi (unProgram . body)

-- | Break out from a loop
break :: Program ()
break = Program Imp.break

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
    fprf :: Handle -> String -> [FunArg Formattable Data] -> r

instance (a ~ ()) => PrintfType (Program a)
  where
    fprf h form as = Program $ Imp.singleE $ FPrintf h form (reverse as)

instance (Formattable a, PrintfType r) => PrintfType (Data a -> r)
  where
    fprf h form as = \a -> fprf h form (ValArg a : as)

-- | Print to a handle. Accepts a variable number of arguments.
fprintf :: PrintfType r => Handle -> String -> r
fprintf h format = fprf h format []

-- | Put a single value to a handle
fput :: Formattable a
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

newObject
    :: String  -- ^ Object type
    -> Program Object
newObject = Program . Imp.newObject

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
-- > import Language.C.Quote.C
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

addExternFun :: forall proxy res . Type res
    => String              -- ^ Function name
    -> proxy res           -- ^ Proxy for expression and result type
    -> [FunArg Type Data]  -- ^ Arguments (only used to determine types)
    -> Program ()
addExternFun fun res args = Program $ Imp.addExternFun fun res' args
  where
    res' = Proxy :: Proxy (Data res)

-- | Declare an external procedure
addExternProc
    :: String              -- ^ Procedure name
    -> [FunArg Type Data]  -- ^ Arguments (only used to determine types)
    -> Program ()
addExternProc proc args = Program $ Imp.addExternProc proc args

-- | Call a function
callFun :: Type a
    => String             -- ^ Function name
    -> [FunArg Any Data]  -- ^ Arguments
    -> Program (Data a)
callFun fun as = Program $ Imp.callFun fun as

-- | Call a procedure
callProc
    :: String             -- ^ Function name
    -> [FunArg Any Data]  -- ^ Arguments
    -> Program ()
callProc fun as = Program $ Imp.callProc fun as

-- | Declare and call an external function
externFun :: Type res
    => String              -- ^ Procedure name
    -> [FunArg Type Data]  -- ^ Arguments
    -> Program (Data res)
externFun fun args = Program $ Imp.externFun fun args

-- | Declare and call an external procedure
externProc
    :: String              -- ^ Procedure name
    -> [FunArg Type Data]  -- ^ Arguments
    -> Program ()
externProc proc args = Program $ Imp.externProc proc args

-- | Get current time as number of seconds passed today
getTime :: Program (Data Double)
getTime = Program Imp.getTime

