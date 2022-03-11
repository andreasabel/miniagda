module TCM where

-- import CallStack
import TraceError

import Control.Monad.State  (StateT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)

data OneOrTwo a = One a | Two a a

data TCContext
data TCState

-- type TypeCheck = StateT TCState (ReaderT TCContext (CallStackT String IO))
type TypeCheck = StateT TCState (ReaderT TCContext (ExceptT TraceError IO))
