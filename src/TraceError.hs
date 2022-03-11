{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module TraceError where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(..))
import Debug.Trace

import Util
import Text.PrettyPrint

data TraceError = Err String | TrErr String TraceError

-- instance Error TraceError where
--     noMsg = Err "no message"
--     strMsg s = Err s

instance Show TraceError where
    show (Err str) = str
    show (TrErr str err) = str ++ "\n/// " ++ show err

throwErrorMsg :: MonadError TraceError m => String -> m a
throwErrorMsg = throwError . Err

newErrorMsg :: (MonadError TraceError m) => m a -> String -> m a
newErrorMsg c s = c `catchError` (\ _ -> throwErrorMsg s)
-- addErrorMsg c s = c `catchError` (\ s' -> throwErrorMsg (s' ++ "\n" ++ s))

-- extend the current error message by n
throwTrace :: MonadError TraceError m => m a -> String -> m a
throwTrace x n = x `catchError` ( \e -> throwError $ TrErr n e)

enter, enterTrace :: MonadError TraceError m => String -> m a -> m a
enter n x = throwTrace x n
enterTrace n x = trace n $ throwTrace x n

enterShow :: (MonadError TraceError m, Show a) => a -> m b -> m b
enterShow n = enter (show n)

enterDoc :: (MonadError TraceError m, Pretty d) => m d -> m a -> m a
enterDoc md cont = do
  d <- md
  enter (render (pretty d)) cont

failDoc :: (MonadError TraceError m) => m Doc -> m a
failDoc d = throwErrorMsg . render =<< d

newErrorDoc :: (MonadError TraceError m) => m a -> m Doc -> m a
newErrorDoc c d = c `catchError` (\ _ -> failDoc d)

errorToMaybe :: (MonadError e m) => m a -> m (Maybe a)
errorToMaybe m = (m >>= return . Just) `catchError` (const $ return Nothing)

errorToBool :: (MonadError e m) => m () -> m Bool
errorToBool m = (m >> return True) `catchError` (\ _ -> return False)

boolToErrorDoc :: (MonadError TraceError m) => m Doc -> Bool -> m ()
boolToErrorDoc _ True  = return ()
boolToErrorDoc d False = failDoc d

boolToError :: (MonadError TraceError m) => String -> Bool -> m ()
boolToError _   True  = return ()
boolToError msg False = throwErrorMsg msg

-- defined in Control.Monad.Error.Class in mtl-2.2.2
-- instance MonadError () Maybe where
--   catchError Nothing k = k ()
--   catchError (Just a) k = Just a
--   throwError () = Nothing

orM :: (MonadError e m) => m a -> m a -> m a
orM m1 m2 = m1 `catchError` (const m2)

-- recoverable errors

data AssertionHandling = Failure | Warning | Ignore
                       deriving (Eq,Ord,Show)

assert' :: (MonadError TraceError m, MonadIO m) => AssertionHandling -> Bool -> String -> m ()
assert' Ignore _      _ = return ()
assert' _       True  _ = return ()
assert' Warning False s = liftIO $ putStrLn $ "warning: ignoring error: " ++ s
assert' Failure False s = throwErrorMsg s

assertDoc' :: (MonadError TraceError m, MonadIO m) => AssertionHandling -> Bool -> m Doc -> m ()
assertDoc' h b md = assert' h b . render =<< md

class Monad m => MonadAssert m where
  assert :: Bool -> String -> m ()
  assertDoc :: Bool -> m Doc -> m ()
  assertDoc b md = assert b . render =<< md
  newAssertionHandling :: AssertionHandling -> m a -> m a
  recoverFail :: String -> m ()
  recoverFail = assert False
  recoverFailDoc :: m Doc -> m ()
  recoverFailDoc = assertDoc False

{-
assert' :: (MonadIO m) => AssertionHandling -> Bool -> String -> m a -> m a
assert' Ignore b s k = k
assert' h True s k = k
assert' Warning False s k = do
  liftIO $ putStrLn s
  k
assert' Failure False s k = fail s

class Monad m => MonadAssert m where
  assert :: Bool -> String -> m a -> m a
  newAssertionHandling :: AssertionHandling -> m a -> m a
-}
