module Feldspar.Concurrent
  ( ThreadId
  , ChanBound
  , Chan
  , Closeable
  , Uncloseable
  , fork
  , forkWithId
  , asyncKillThread
  , killThread
  , waitThread
  , newChan
  , newCloseableChan
  , readChan
  , writeChan
  , closeChan
  , lastChanReadOK
  ) where



import Language.Embedded.Concurrent (ThreadId, ChanBound, Chan, Closeable, Uncloseable)
import qualified Language.Embedded.Concurrent as Imp

import Feldspar.IO



-- | Fork off a computation as a new thread.
fork :: Program () -> Program ThreadId
fork = Program . Imp.fork . unProgram

-- | Fork off a computation as a new thread, with access to its own thread ID.
forkWithId :: (ThreadId -> Program ()) -> Program ThreadId
forkWithId f = Program $ Imp.forkWithId (unProgram . f)

-- | Forcibly terminate a thread, then continue execution immediately.
asyncKillThread :: ThreadId -> Program ()
asyncKillThread = Program . Imp.asyncKillThread

-- | Forcibly terminate a thread. Blocks until the thread is actually dead.
killThread :: ThreadId -> Program ()
killThread = Program . Imp.killThread

-- | Wait for a thread to terminate.
waitThread :: ThreadId -> Program ()
waitThread = Program . Imp.waitThread

-- | Create a new channel. Writing a reference type to a channel will copy the
--   /reference/ into the queue, not its contents.
--
--   We'll likely want to change this, actually copying arrays and the like
--   into the queue instead of sharing them across threads.
newChan :: Type a => Data ChanBound -> Program (Chan Uncloseable a)
newChan = Program . Imp.newChan

newCloseableChan :: Type a => Data ChanBound -> Program (Chan Closeable a)
newCloseableChan = Program . Imp.newCloseableChan

-- | Read an element from a channel. If channel is empty, blocks until there
--   is an item available.
--   If 'closeChan' has been called on the channel *and* if the channel is
--   empty, @readChan@ returns an undefined value immediately.
readChan :: Type a => Chan t a -> Program (Data a)
readChan = Program . Imp.readChan

-- | Write a data element to a channel.
--   If 'closeChan' has been called on the channel, all calls to @writeChan@
--   become non-blocking no-ops and return @False@, otherwise returns @True@.
writeChan :: Type a => Chan t a  -> Data a -> Program (Data Bool)
writeChan c = Program . Imp.writeChan c

-- | When 'readChan' was last called on the given channel, did the read
--   succeed?
--   Always returns @True@ unless 'closeChan' has been called on the channel.
--   Always returns @True@ if the channel has never been read.
lastChanReadOK :: Chan Closeable a -> Program (Data Bool)
lastChanReadOK = Program . Imp.lastChanReadOK

-- | Close a channel. All subsequent write operations will be no-ops.
--   After the channel is drained, all subsequent read operations will be
--   no-ops as well.
closeChan :: Chan Closeable a -> Program ()
closeChan = Program . Imp.closeChan

