module Feldspar.IO.Mutable where



import Feldspar
import Feldspar.Mutable
import Feldspar.IO.Frontend



liftMut :: Syntax a => M a -> Program a
liftMut = return . runMutable

