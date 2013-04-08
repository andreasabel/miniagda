module SPos (nocc,noccFromBool) where

import Abstract
import Polarity
import Value
import TCM
import Eval
import TraceError
import Util

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
-- import Control.Monad.HT (andLazy) -- now also in Util.hs

import Debug.Trace

{-
traceSPos msg a = a -- trace msg a
traceSPosM msg = return () -- traceM msg
-}
traceSPos msg a = trace msg a
traceSPosM msg = traceM msg



-- nocc (used by SPos and TypeCheck)
-- check that a does not occur in tv
-- a may be a "atomic value" i.e not pi , lam , app , or succ
nocc :: Int -> Val -> TVal -> TypeCheck Bool
nocc k a tv = do
  -- traceM ("noccRecArg " ++ show tv)
  tv <- whnfClos tv
  case tv of
    a' | a == a'                 -> return False
    VQuant Pi x dom env b        -> nocc k a (typ dom) `andLazy` do
       nocc (k+1) a =<< whnf (update env x (VGen k)) b
    VLam x env b                 -> nocc (k+1) a =<< whnf (update env x (VGen k)) b
    VSucc v                      -> nocc k a v
    VApp v1 []                   -> return True -- because  VApp v1 [] != a
    VApp v1 vl                   -> andM $ map (nocc k a) $ VApp v1 [] : vl
    VRecord AnonRec rs           -> andM $ map (nocc k a . snd) rs
    VRecord (NamedRec co c _ _) rs -> andM $
      nocc k a (vCon co c) : map (nocc k a . snd) rs
    VGen{}                       -> return $ True
    VZero                        -> return $ True
    VInfty                       -> return $ True
    VMax vl                      -> andM $ map (nocc k a) vl
    VPlus vl                     -> andM $ map (nocc k a) vl
    VSort (CoSet v)              -> nocc k a v
    VSort{}                      -> return $ True
    VSing v tv                   -> nocc k a tv
    VUp v tv                     -> nocc k a v
    VIrr                         -> return $ True
    VCase v _ env cls            -> andM $
      nocc k a v : map (nocc k a . snd) (envMap env)
    _                            -> fail $ "internal error: NYI: nocc " ++ show (k,a,tv)

noccFromBool :: Bool -> Pol
noccFromBool True  = Polarity.Const
noccFromBool False = mixed


------------------------------ RETIRED CODE --------------------------
{- BEGIN RETIRED


-----------------------------------
-- check that recursive data argument n and the spos declared parameter variables are only used strictly positivly
-- return a list of booleans; an entry is True if the corresponding argument
-- containts a recursive occurrence
sposConstructor :: Name -> Int -> [Pos] -> TVal -> TypeCheck [Bool]
sposConstructor n k sp tv = traceSPos ("sposConstructor " ++ show n ++ " " ++ show sp ++ " " ++ show tv) $
  loop k tv where
    loop k tv
     = case tv of
         VPi dec x av env b -> do
              spr <- spos 0 (vDat n) av
              spv <- sposVals (posGen 0 sp) av
              case (isSPos spr, isSPos spv) of
                (True,True) -> do bv <- whnf (update env x (VGen k)) b
                                  occs <- loop (k+1) bv
                                  return $ (spr == SPos) : occs
                (False,_) -> throwErrorMsg "rec. arg not strictly positive"
                (True,False) -> throwErrorMsg $ "parameter not strictly positive in " ++ show av
         _ -> return []

sposVals :: [Val] -> TVal -> TypeCheck Pos
sposVals vals tv = do sl <- mapM (\i -> spos 0 i tv) vals
                      return $ foldl polAnd NOcc sl

posGen :: Int -> [Pos] -> [Val]
posGen i [] = []
posGen i (p:pl) = case p of
                    SPos -> (VGen i) : (posGen (i+1) pl)
                    _ -> posGen (i+1) pl

posArgs :: [Val] -> [Pos] -> ([Val],[Val])
posArgs vl pl = let l = zip vl pl
                    l1 = [ v | (v,SPos) <- l]
                    l2 = [ v | (v,Mixed) <- l]
                 in
                   (l1,l2)

-- check that a only occurs strictly pos tv
-- a may be a "atomic value" ie not pi , lam , app , or succ
-- Results are: SPos, Mixed, or NOcc
spos :: Int -> Val -> TVal -> TypeCheck Pos
spos k a tv = traceSPos ("spos " ++ show a ++ " in " ++ show tv) $
 do tv <- whnfClos tv
    (case tv of
         a' | a == a' -> return SPos
         VPi dec x av env b ->
             do no <- nocc k a av
                case no of
                  True -> do
                      bv <- whnf (update env x (VGen k)) b
                      spos (k+1) a bv
                  False  -> return Mixed
         VLam x env b -> do
                 bv <- whnf (update env x (VGen k)) b
                 spos (k+1) a bv
         VSucc v -> spos k a v
         VApp (VDef id) [] -> return $ NOcc
         VApp (VDef id) vl -> do
               sige <- lookupSymb (name id)
               case sige of
                 (DataSig { numPars = p, positivity = pos }) ->
                     do let (pparams,nparams) = posArgs vl pos
                        let rest = drop p vl
                        sl <- mapM (spos k a) pparams
                        nl <- mapM (nocc k a) (nparams ++ rest)
                        mp <- spos k a (VDef id `VApp` [])
                        return $ foldl polAnd (noccFromBool $ and nl) (mp:sl)
                 _ -> do nl <- mapM (nocc k a) vl
                         return $ noccFromBool $ and nl
{- Superfluous clause
         VApp v1 vl -> do n <- nocc k a v1
                          nl <- mapM (nocc k a) vl
                          return $ noccFromBool $ n && and nl
 -}
         _ -> nocc k a tv >>= return . noccFromBool)
      >>= \ res -> traceSPos ("spos " ++ show a ++ " in " ++ show tv ++ " returns " ++ show res) $ return res

END RETIRED -}
