module TCM where

-- import CallStack
import TraceError

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

data TCContext
data TCState

-- type TypeCheck = StateT TCState (ReaderT TCContext (CallStackT String IO)) 
type TypeCheck = StateT TCState (ReaderT TCContext (ErrorT TraceError IO)) 