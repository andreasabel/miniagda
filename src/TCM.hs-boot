module TCM where

-- import CallStack
import TraceError

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

data OneOrTwo a = One a | Two a a

data TCContext
data TCState

-- type TypeCheck = StateT TCState (ReaderT TCContext (CallStackT String IO))
type TypeCheck = StateT TCState (ReaderT TCContext (ExceptT TraceError IO))
