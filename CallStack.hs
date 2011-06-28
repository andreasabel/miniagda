-- ABANDONED

{-# LANGUAGE UndecidableInstances #-} 
module CallStack where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State

{- This implements a call-stack for error messages and debugging.

The idea is that on an error, you want the "call stack", the nested
sequence of actions that lead to the error.  Previous successful
actions are irrelevant, so the usual logging produces to much junk
information.

Marking a call can be done by

  enter :: string -> Callstack string a -> Callstack string a

so, upon entry into a procedure you write

  proc a b c = enter "proc a b c" $ do
    code_or_proc

Invoking an error prepends the whole call stack to the error message

  fail "error message"

-}

class Monad m => MonadCall s m | m -> s where
  enter :: s -> m a -> m a

newtype CallStack s a = CallStack { runCallStack :: [s] -> Either String a }

instance Show s => Monad (CallStack s) where
  return a             = CallStack $ \ stack -> return a 
  (CallStack ma) >>= k = CallStack $ \ stack -> do 
                           a <- ma stack
                           runCallStack (k a) stack
  fail msg             = CallStack $ \ stack -> fail $ 
    foldr (\ a b -> show a ++ "\n///" ++ b) msg stack
  
instance Show s => MonadCall s (CallStack s) where
  enter msg k = CallStack $ \ stack -> runCallStack k (msg:stack)



newtype CallStackT s m a = CallStackT { runCallStackT :: [s] -> ErrorT String m a }

instance (Show s, Monad m) => Monad (CallStackT s m) where
  return a             = CallStackT $ \ stack -> return a 
  (CallStackT ma) >>= k = CallStackT $ \ stack -> do 
                           a <- ma stack
                           runCallStackT (k a) stack
  fail msg             = CallStackT $ \ stack -> fail $ 
    foldr (\ a b -> show a ++ "\n///" ++ b) msg stack

instance MonadTrans (CallStackT s) where
  lift ma = CallStackT $ \ stack -> lift ma 

instance (Show s, Monad m) => MonadError String (CallStackT s m) where
  throwError = fail
  catchError ma k = CallStackT $ \ stack -> 
    catchError (runCallStackT ma stack) $ \ msg -> runCallStackT (k msg) stack

instance (Show s, MonadIO m) => MonadIO (CallStackT s m) where
  liftIO ia = CallStackT $ \ stack -> liftIO ia

instance (Show s, Monad m) => MonadCall s (CallStackT s m) where
  enter msg k = CallStackT $ \ stack -> runCallStackT k (msg:stack)

{- SOMETHING' WRONG
instance MonadCall s m => MonadCall s (StateT st m) where
  enter msg ma = StateT $ \ st -> enter msg ma
-}

-- functions from TraceError

throwErrorMsg m = throwError m 

-- newErrorMsg :: (MonadError TraceError m) => m a -> String -> m a
newErrorMsg c s = c `catchError` (\ _ -> throwErrorMsg s)
-- addErrorMsg c s = c `catchError` (\ s' -> throwErrorMsg (s' ++ "\n" ++ s))

-- extend the current error message by n
-- throwTrace x n = x `catchError` ( \e -> throwError $ TrErr n e) 
throwTrace ma msg = enter msg ma

errorToBool :: (MonadError e m) => m () -> m Bool
errorToBool m = (m >> return True) `catchError` (\ _ -> return False)