module SPos (sposConstructor,posArgs) where

import Abstract
import Value

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Debug.Trace

-----------------------------------
-- check that recursive Data argument n and the spos declared parameter variables are only used strictly positvly

sposConstructor :: Name -> Int -> [Pos] -> TVal -> TypeCheck ()
sposConstructor n k spos tv = 
    case tv of
         VPi x av (VClos env b) -> do 
                                    spr <- sposRecArg 0 n av
                                    spv <- sposVars (posGen 0spos) av
                                    case (spr,spv) of
                                      (True,True) -> do bv <- vclos (updateV env x (VGen k)) b
                                                        bv <- whnf bv
                                                        sposConstructor n (k+1) spos bv
                                      (False,_) -> throwErrorMsg "rec. arg not strictly positive"
                                      (True,False) -> throwErrorMsg "parameter not strictly positive"
         _ -> return ()

posGen :: Int -> [Pos] -> [Int]
posGen i [] = []
posGen i (p:pl) = case p of
                    SPos -> i : (posGen (i+1) pl)
                    NSPos -> posGen (i+1) pl

-- check wether variables appear only strictly pos in type
sposVars :: [Int] -> TVal -> TypeCheck Bool
sposVars vars tv = do sl <- mapM (\i -> sposVar 0 i tv) vars
                      return $ and sl

-- checks wether variable is used strictly positiv in type tv
sposVar :: Int -> Int -> TVal -> TypeCheck Bool
sposVar k i tv = -- trace ("sposVar " ++ show tv) $
   case tv of
         VPi x av (VClos env b) ->
              do 
                 n <- noccVar k i av
                 case n of
                   True -> do
                        bv <- vclos (updateV env x (VGen k)) b
                        bv <- whnf bv
                        sposVar (k+1) i bv
                   False -> return False
         (VApp (VDef m) cls) -> do
                 do sig <- get
                    vl <- mapM whnf cls
                    case (lookupSig m sig) of
                      (DataSig p pos _ _ _ ) ->
                          do vl <- mapM whnf cls
                             let (pparams,nparams) = posArgs vl pos
                             let rest = drop p vl
                             sl <- mapM (sposVar k i) pparams
                             nl <- mapM (noccVar k i) (nparams ++ rest)
                             return $ and sl && and nl
                      _ -> do nl <- mapM (noccVar k i) vl
                              return $ and nl
         (VApp v cls) -> do
                 no <- noccVar k i v
                 vl <- mapM whnf cls
                 nl <- mapM (noccVar k i) vl
                 return $ no && and nl
         (VSucc v) -> sposVar k i v
         _ -> return $ True


-- check that i does not occur in tv
noccVar :: Int -> Int -> TVal -> TypeCheck Bool
noccVar k i tv = -- trace ("noccVar " ++ show tv) $
       case tv of
         VPi x av (VClos env b) -> 
             do n <- noccVar k i av
                case n of 
                  True -> do
                      bv <- vclos (updateV env x (VGen k)) b
                      bv <- whnf bv
                      noccVar (k+1) i bv
                  False  -> return False
         VApp v1 cls -> do sp <- noccVar k i v1
                           vl <- mapM whnf cls
                           nl <- mapM (noccVar k i) vl
                           return $ sp && and nl
         VGen i' -> return $ i /= i'
         VSucc v -> noccVar k i v
         _ -> return True


---

-- checks that rec. argument is used strictly positiv in type tv
sposRecArg :: Int -> Name -> TVal -> TypeCheck Bool
sposRecArg k n tv = -- trace ("sposRecArg " ++ show tv) $
       case tv of
         VPi x av (VClos env b) ->
              do no <- noccRecArg k n av
                 case no of
                   True -> do
                        bv <- vclos (updateV env x (VGen k)) b
                        bv <- whnf bv
                        sposRecArg (k+1) n bv
                   False -> return False
         (VApp (VDef m) cls) -> do
                 do sig <- get
                    vl <- mapM whnf cls
                    case (lookupSig m sig) of
                      (DataSig p pos _ _ _) ->
                          do let (pparams,nparams) = posArgs vl pos
                             let rest = drop p vl
                             sl <- mapM (sposRecArg k n) pparams
                             nl <- mapM (noccRecArg k n) (nparams ++ rest)
                             return $ and sl && and nl
                      _ -> do sl <- mapM (sposRecArg k n) vl
                              return $ and sl
         (VApp v cls) -> do
                 sp <- sposRecArg k n v
                 vl <- mapM whnf cls
                 nl <- mapM (noccRecArg k n) vl
                 return $ sp && and nl
         (VSucc v) -> sposRecArg k n v
         _ -> return $ True

-- return strictly positiv arguments in left, other in right list
posArgs :: [Val] -> [Pos] -> ([Val],[Val])
posArgs vl pl = let l = zip vl pl
                    l1 = [ v | (v,SPos) <- l]
                    l2 = [ v | (v,NSPos) <- l]
                 in
                   (l1,l2)

-- check that n does not occur in tv
noccRecArg :: Int -> Name -> TVal -> TypeCheck Bool
noccRecArg k n tv = -- trace ("noccRecArg " ++ show tv) 
     case tv of
         VPi x av (VClos env b) -> 
             do no <- noccRecArg k n av
                case no of 
                  True -> do
                      bv <- vclos (updateV env x (VGen k)) b
                      bv <- whnf bv
                      noccRecArg (k+1) n bv
                  False  -> return False
         VApp (VDef m) vl | n == m -> return False
         VApp v1 cls -> do no <- noccRecArg k n v1
                           vl <- mapM whnf cls
                           nl <- mapM (noccRecArg k n) vl
                           return $ no && and nl
         VSucc v -> noccRecArg k n v
         VDef m -> return $ m /= n
         _ -> return True

