module TraceError where

import Control.Monad.Error

data TraceError = Err String | TrErr String TraceError

instance Error TraceError where
    noMsg = Err "no message"
    strMsg s = Err s

instance Show TraceError where
    show (Err str) = str
    show (TrErr str err) = str ++ " -> \n" ++ show err 

throwErrorMsg m = throwError (Err m) 

throwTrace x n = x `catchError` ( \e -> throwError $ TrErr n e) 

