{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
      PatternGuards, TupleSections, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker where

import Prelude hiding (null)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad          (foldM, forM, forM_, liftM, unless, when, zipWithM, zipWithM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State    (runStateT, get, gets, put)
import Control.Monad.Except   (runExceptT, catchError)
import Control.Monad.Reader   (runReaderT, ask, asks, local)

import qualified Data.List as List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

-- import Debug.Trace (trace)

import qualified Text.PrettyPrint as PP

import Util

import Abstract hiding (Substitute)
import Polarity as Pol
import Value
import TCM
import Eval
import Extract
-- import CallStack
import PrettyTCM
import TraceError

import Termination

-- import Completness

traceCheck, traceSing, traceAdm :: String -> a -> a
traceCheckM, traceSingM, traceAdmM :: Monad m => String -> m ()


traceCheck msg a = a -- trace msg a
traceCheckM msg = return () -- traceM msg
{-
traceCheck msg a = trace msg a
traceCheckM msg = traceM msg
-}

traceSing msg a = a -- trace msg a
traceSingM msg = return () -- traceM msg
{-
traceSing msg a = trace msg a
traceSingM msg = traceM msg
-}

traceAdm msg a = a -- trace msg a
traceAdmM msg = return () -- traceM msg
{-
traceAdm msg a = trace msg a
traceAdmM msg = traceM msg
-}

{- DEAD CODE
runWhnf :: Signature -> TypeCheck a -> IO (Either TraceError (a,Signature))
runWhnf sig tc = (runExceptT (runStateT tc  sig))
-}

-- | Entry point for normalization without controlling unfolding.
doNf :: Signature -> Expr -> IO (Either TraceError (Expr, TCState))
doNf sig e = runExceptT $
  runReaderT (runStateT (whnf emptyEnv e >>= reify) (initWithSig sig)) $
    emptyContext False

-- | Entry point for weak head evaluation without controlling unfolding.
doWhnf :: Signature -> Expr -> IO (Either TraceError (Val, TCState))
doWhnf sig e = runExceptT $
  runReaderT (runStateT (whnf emptyEnv e >>= whnfClos) (initWithSig sig)) $
    emptyContext False


-- top-level functions -------------------------------------------

-- | Type-checker entrypoint.
typeCheck
  :: Bool           -- ^ Control unfolding of definitions?
  -> [Declaration]  -- ^ Scope-checked declarations to type check.
  -> IO (Either TraceError ([EDeclaration], TCState))
typeCheck controlUnfolding dl =
  runTypeCheck controlUnfolding initSt $ typeCheckDecls dl

-- | Running a 'TypeCheck' computation in the empty context.
runTypeCheck
  :: Bool          -- ^ Control unfolding of definitions?
  -> TCState       -- ^ Initial type checker state.
  -> TypeCheck a   -- ^ Computation.
  -> IO (Either TraceError (a, TCState))
runTypeCheck controlUnfolding st tc =
  runExceptT (runReaderT (runStateT tc st) $ emptyContext controlUnfolding)

-- checking top-level declarations -------------------------------

echo :: MonadIO m => String -> m ()
echo = liftIO . putStrLn

echoR :: MonadIO m => String -> m ()
echoR = echo
-- echoR s = echo $ "R> " ++ s

echoTySig :: (Show n, MonadIO m) => n -> Expr -> m ()
echoTySig n t = return () -- echo $ "I> " ++ n ++ " : " ++ show t

echoKindedTySig :: (Show n, MonadIO m) => Kind -> n -> Expr -> m ()
echoKindedTySig ki n t = echo $ prettyKind ki ++ "  " ++ show n ++ " : " ++ show t

echoKindedDef :: (Show n, MonadIO m) => Kind -> n -> Expr -> m ()
echoKindedDef ki n t = echo $ prettyKind ki ++ "  " ++ show n ++ " = " ++ show t

echoEPrefix :: String
echoEPrefix = "E> "

echoTySigE :: (Show n, MonadIO m) => n -> Expr -> m ()
echoTySigE n t = echo $ echoEPrefix ++ show n ++ " : " ++ show t

echoDefE :: (Show n, MonadIO m) => n -> Expr -> m ()
echoDefE n t = echo $ echoEPrefix ++ show n ++ " = " ++ show t

-- the type checker returns pruned (extracted) terms
-- with irrelevant subterms replaced by Irr
typeCheckDecls :: [Declaration] -> TypeCheck [EDeclaration]
typeCheckDecls []     = return []
typeCheckDecls (d:ds) = do
  de  <- typeCheckDeclaration d
  dse <- typeCheckDecls ds
  return (de ++ dse)

-- since a data declaration generates destructor declarations
-- we need to return a list here
typeCheckDeclaration :: Declaration -> TypeCheck [EDeclaration]
typeCheckDeclaration (OverrideDecl Check ds) = do
  st <- get
  _ <- typeCheckDecls ds
  put st             -- forget the effect of these decls
  return []
typeCheckDeclaration (OverrideDecl Fail ds) = do
  st <- get
  r <- (typeCheckDecls ds >> return True) `catchError`
        (\ s -> do liftIO $ putStrLn ("block fails as expected, error message:\n" ++ show s)
                   return False)
  if r then throwErrorMsg "unexpected success" else do
    put st
    return []

typeCheckDeclaration (OverrideDecl TrustMe ds) =
  newAssertionHandling Warning $ typeCheckDecls ds

typeCheckDeclaration (OverrideDecl Impredicative ds) =
  goImpredicative $ typeCheckDecls ds

typeCheckDeclaration (RecordDecl n tel t0 c fields) =
  -- just one "mutual" declaration
  checkingMutual (Just $ DefId DatK $ QName n) $ do
    result <- typeCheckDataDecl n NotSized CoInd [] tel t0 [c] fields
    checkPositivityGraph
    return result

typeCheckDeclaration (DataDecl n sz co pos0 tel t0 cs fields) =
  -- just one "mutual" declaration
  checkingMutual (Just $ DefId DatK $ QName n) $ do
    result <- typeCheckDataDecl n sz co pos0 tel t0 cs fields
    checkPositivityGraph
    return result

typeCheckDeclaration (LetDecl eval n tel mt unf e) = enter (show n) $ do
{- MOVED to checkLetDef
  (tel, (vt, te, Kinded ki ee)) <- checkTele tel $ checkOrInfer neutralDec e mt
  te <- return $ teleToType tel te
  ee <- return $ teleLam tel ee
  vt <- whnf' te
-}
  (vt, te, Kinded ki ee) <- checkLetDef neutralDec tel mt unf e
  rho <- getEnv -- is emptyEnv
  -- TODO: solve size constraints
  -- does not work with emptyEnv
  -- [te, ee] <- solveAndModify [te, ee] rho  -- solve size constraints
  let v = mkClos rho ee -- delay whnf computation
  -- v  <- whnf' ee -- WAS: whnf' e'
  addSig n (LetSig vt ki v unf $ undefinedFType $ QName n)    -- late (var -> expr) binding, but ok since no shadowing
--  addSig n (LetSig vt e')    -- late (var -> expr) binding, but ok since no shadowing
  echoKindedTySig ki n te
--  echoTySigE n te
--  echoDefE   n ee
  echoKindedDef ki n ee
  return [LetDecl eval n emptyTel (Just te) unf ee]

typeCheckDeclaration d@(PatternDecl x xs p) = do
{- WHY DOES THIS NOT TYPECHECK?
  let doc = (PP.text "pattern") <+> (PP.hsep (List.map Util.pretty (x:xs))) <+> PP.equals <+> Util.pretty p
  echo $ PP.render $ doc
-}
  echo $ "pattern " ++ Util.showList " " show (x:xs) ++ " = " ++ show p
  v <- whnf' $ foldr (Lam defaultDec) (patternToExpr p) xs
  addSig x (PatSig xs p v)
  return [d]

typeCheckDeclaration (MutualFunDecl False co funs) =
  -- traceCheck ("type checking a function block") $
  do
    funse <- typeCheckFuns co funs
    return $ [MutualFunDecl False co funse]

typeCheckDeclaration (MutualFunDecl True co funs) =
  -- traceCheck ("type checking a block of measured function") $
  do
    funse <- typeCheckMeasuredFuns co funs
    return $ [MutualFunDecl False co funse]

typeCheckDeclaration (MutualDecl measured ds) = do
  -- first check type signatures
  -- we add the typings into the context, not the signature
  ktss <- typeCheckMutualSigs ds
  -- register the mutually defined names
  let ns = for ktss $ \ (Kinded _ (TypeSig n _)) -> n
      addMutualNames = local $ \ e -> e { mutualNames = ns ++ mutualNames e }
  -- then check bodies
  -- we need to construct a positivity graph
  edss <- addKindedTypeSigs ktss $ addMutualNames $
    zipWithM (typeCheckMutualBody measured) (map (predKind . kindOf) ktss) ds
  -- check and reset positivity graph
  checkPositivityGraph
  return $ concat edss


-- check signatures of a flattened mutual block
typeCheckMutualSigs :: [Declaration] -> TypeCheck [Kinded (TySig TVal)]
typeCheckMutualSigs [] = return []
typeCheckMutualSigs (d:ds) = do
  kts@(Kinded ki (TypeSig n tv)) <- typeCheckMutualSig d
  new' n (Domain tv ki defaultDec) $ do
    ktss <- typeCheckMutualSigs ds
    return $ kts : ktss

typeCheckSignature :: TySig Type -> TypeCheck (Kinded (TySig TVal))
typeCheckSignature (TypeSig n t) = do
  echoTySig n t
  Kinded ki te <- checkType t
  tv <- whnf' te
  return $ Kinded (predKind ki) $ TypeSig n tv

typeCheckMutualSig :: Declaration -> TypeCheck (Kinded (TySig TVal))
typeCheckMutualSig (LetDecl ev n tel (Just t) _unfolds e) =
  typeCheckSignature $ TypeSig n $ teleToType tel t
typeCheckMutualSig (DataDecl n sz co pos tel t cs fields) = do
  Kinded ki ts <- typeCheckSignature (TypeSig n (teleToType tel t))
  return $ Kinded ki ts
typeCheckMutualSig (FunDecl _co (Fun ts _n _ar _unfolds _cls)) =
  typeCheckSignature ts
typeCheckMutualSig (OverrideDecl TrustMe [d]) =
  newAssertionHandling Warning $ typeCheckMutualSig d
typeCheckMutualSig (OverrideDecl Impredicative [d]) =
  goImpredicative $ typeCheckMutualSig d
typeCheckMutualSig d = throwErrorMsg $ "typeCheckMutualSig: panic: unexpected declaration " ++ show d

-- typeCheckMutualBody measured kindCandidate
typeCheckMutualBody :: Bool -> Kind -> Declaration -> TypeCheck [EDeclaration]
typeCheckMutualBody measured _ (DataDecl n sz co pos tel t cs fields) = do
  -- set name of mutual thing whose body we are checking
  checkingMutual (Just $ DefId DatK $ QName n) $
    --
    typeCheckDataDecl n sz co pos tel t cs fields
typeCheckMutualBody measured@False ki (FunDecl co fun@(Fun ts@(TypeSig n t) _n _ar _unfolds _cls)) = do
  checkingMutual (Just $ DefId FunK $ QName n) $ do
    fun' <- typeCheckFunBody co ki fun
    return $ [FunDecl co fun']

typeCheckDataDecl :: Name -> Sized -> Co -> [Pol] -> Telescope -> Type -> [Constructor] -> [Name] -> TypeCheck [EDeclaration]
typeCheckDataDecl n sz co pos0 tel0 t0 cs0 fields = enter (show n) $
 (do -- sig <- gets signature
     let params = size tel0
     -- in case we are dealing with a sized type, check that
     -- the polarity annotation (if present) at the size arg. is correct.
     (p', pos, t) <- do
       case sz of
         Sized    -> do
           let polsz = if co==Ind then Pos else Neg
           t <- case t0 of
             Quant Pi (TBind x (Domain domt ki dec)) b | isSize domt ->
               case (polarity dec) of
                 -- insert correct polarity annotation if none was there
                 pol | pol `elem` [Param,Rec] -> return $ Quant Pi (TBind x $ Domain tSize kSize $ setPol polsz dec) b
                 pol | pol == polsz -> return t0
                 pol -> throwErrorMsg $ "sized type " ++ show n ++ " has wrong polarity annotation " ++ show pol ++ " at Size argument, it should be " ++ show polsz
             t0 -> return t0
           return (params + 1, pos0 ++ [polsz], t)
         NotSized -> do
           -- Warn if it looks like a sized type, but there was no keyword "sized".
           case t0 of
             Quant Pi (TBind x (Domain domt ki dec)) b | isSize domt -> do
               traceM $ "Warning: data " ++ show n ++
                 " looks like you want to define a sized type, did you forget keyword `sized`?"
             _ -> return ()
           return (params, pos0, t0)
     -- compute full type signature (including parameter telescope)
     let dt = (teleToType tel0 t)
     echoTySig n dt
     {- mmh, this does not work,  e.g.  data Id (A : Set)(a : A) : A -> Set
        then A -> Set is not distinguishable from Set -> Set (GADT)
        unclear what to do...
     dte <- checkTele tel $ \ tele -> do
       te <- checkSmallType t
       return (teleToType tele te)
      -}
     -- get the target sort ds of the datatype
     Kinded ki0 (ds, dte) <- checkDataType p' dt -- TODO?: use above code?
     let ki = dataKind ki0
     echoKindedTySig ki n dte
--     echoTySigE n dte
     v <- whnf emptyEnv dte
     Just fkind <- extractKind v
     -- get the updated telescope which contains the kinds
     let (tel, dtcore) = typeToTele' params dte
     -- compute the constructor telescopes
     cs0 <- mapM (insertConstructorTele tel dtcore) cs0
     let cis = analyzeConstructors co n tel cs0
     let cs  = map reassembleConstructor cis
     addSig n (DataSig { numPars = params
                       , positivity = pos
                       , isSized = sz
                       , isCo = co
                       , symbTyp = v
                       , symbolKind = ki
                       , constructors = cis
                       , etaExpand = False
                       , isTuple = False
-- if cs==[] then Just [] else Nothing
{- OLD CODE
                       , constructors = map namePart cs
                       -- at first, do not add destructors, get them out later
                       , destructors  = Nothing
                       , isFamily = t /= Set  -- currently UNUSED
 -}
                       , extrTyp = fkind
                       })
     when (sz == Sized) $
           szType co params v

     (isRecList, kcse) <- liftM unzip $
       mapM (typeCheckConstructor n dte sz co pos tel) cs

     -- compute the kind of the data type from the kinds of the
     -- constructor arguments  (mmh, DOES NOT WORK FOR MUTUAL DATA!)
     let newki = case (foldl unionKind NoKind (map kindOf kcse)) of
          NoKind  -> kType -- no non-rec constructor arguments
          AnyKind -> AnyKind
          Kind s s' -> Kind (Set Zero) s' -- a data type is always also a type
     -- echoKindedTySig newki n dte -- 2012-01-26 disabled (repetitive)

     -- solve for size variables
     sol <- solveConstraints
     -- TODO: substitute
     resetConstraints

     -- add destructors only for the constructors that are non-overlapping
     let decls = concat $ map mkDestrs cis
         -- cEtaExp = True means that all field names are present
         -- and constructor is not overlapping with others
         mkDestrs ci | cEtaExp ci = concat $ map mkDestr (cFields ci)
                     | otherwise  = []
         mkDestr fi =
          case (fClass fi) of
             Field (Just (ty, arity, cl)) | not (erased $ fDec fi) && not (emptyName $ fName fi) ->
               let n' = fName fi
                   n  = internal n'
               in
               [MutualFunDecl False Ind [Fun (TypeSig n ty) n' arity [] [cl]]]
             _ -> []

     when (not (null decls)) $
        traceCheckM $ "generated destructors: " ++ show decls
     declse <- mapM (\ d@(MutualFunDecl False co [Fun (TypeSig n t) _n' _ar _unfolds cls]) -> do
                       -- echo $ "G> " ++ showFun co ++ " " ++ show n ++ " : " ++ show t
                       -- echo $ "G> " ++ PP.render (prettyFun n cls)
                       checkingMutual Nothing $ typeCheckDeclaration d)
                 decls

     -- decide whether to eta-expand at this type
     -- all patterns need to be proper and non-overlapping
     -- at least one constructor needs to be eta-expandable
     let isPatIndFam = all (\ ci -> fst (cPatFam ci) /= NotPatterns && cEtaExp ci) cis
--                    && not (or overlapList)
     -- do not eta-expand recursive constructors (might not terminate)
     let disableRec ci {-ov-} rec' = ci
          { cRec    = rec'
          , cEtaExp =  cEtaExp ci               -- all destructors present
           && fst (cPatFam ci) /= NotPatterns -- proper pattern to compute indices
--           && not ov                          -- non-overlapping
           && not (co==Ind && rec') }         -- non-recursive
     let cis' = zipWith disableRec cis {-overlapList-} isRecList
     let typeEtaExpandable = isPatIndFam && (null cis || any cEtaExp cis')
     traceEtaM $ "data " ++ show n ++ " eta-expandable " ++ show typeEtaExpandable ++ " constructors " ++ show cis'
     modifySig n (\ dataSig ->
                      dataSig { symbolKind = newki
                              , etaExpand = typeEtaExpandable
                              , constructors = cis'
                              , isTuple = length cis' >= 1 && isPatIndFam
                              })
     -- compute extracted data decl
     let (tele, te) = typeToTele' (size tel) dte
     return $ (DataDecl n sz co pos tele te (map valueOf kcse) fields) : concat declse

   ) -- `throwTrace` n  -- in case of an error, add name n to the trace


insertConstructorTele :: Telescope -> Type -> Constructor -> TypeCheck Constructor
insertConstructorTele dtel dt c@(Constructor n Nothing t) = return c
insertConstructorTele dtel dt c@(Constructor n Just{}  t) = do
  res <- computeConstructorTele dtel dt t
  return $ Constructor n (Just res) t

-- | @computeConstructorTele dtel t = return ctel@
--   Computes the constructor telescope from the target.
computeConstructorTele :: Telescope -> Type -> Type -> TypeCheck (Telescope, [Pattern])
computeConstructorTele dtel dt t = do
  -- target is data name applied to parameters and indices
  let (_, target) = typeToTele t
      (_, es)     = spineView target
      pars = take (size dtel) es
  (cxt, ps) <- checkConstructorParams pars  =<< whnf' (teleToType dtel dt)
  (,ps) . setDec (Dec Param) <$> do local (const cxt) $ contextToTele cxt

-- | @checkConstructorParams pars tv = return cxt@
--   Checks that parameters @pars@ are patterns elimating the datatype @tv@.
--   Returns a context @cxt@ that binds the pattern variables in
--   left-to-right order.
checkConstructorParams :: [Expr] -> TVal -> TypeCheck (TCContext, [Pattern])
checkConstructorParams es tv = do
  -- for now, we only allow patterns in parameters
  -- could be extended to unifyable expressions in general
  ps <- mapM (\ e -> maybe (errorParamNotPattern e) return $ exprToPattern e) es
  -- no goals from dot patterns, no absurd pattern
  ([],_,cxt,_,_,_,False) <- checkPatterns defaultDec [] emptySub tv ps
  return (cxt, ps)

  where
    errorParamNotPattern e = throwErrorMsg $
      "expected parameter to be a pattern, but I found " ++ show es

-- |
--   Precondition: @ce@ is included in the current context.
contextToTele :: TCContext -> TypeCheck Telescope
contextToTele ce = do
  let n     :: Int
      n     = len (context ce)           -- context length
      delta :: Map Int (OneOrTwo Domain)
      delta = cxt (context ce)           -- types for dB levels
      names :: Map Int Name
      names = naming ce                  -- names for dB levels
  -- traverse the context from left to right
  Telescope <$> do
    forM [0..n-1] $ \ k -> do
      x       <- lookupM k names
      One dom <- lookupM k delta
      TBind x <$> Traversable.traverse toExpr dom

-- | @typeCheckConstructor d dt sz co pols tel (TypeSig c t)@
--
--   returns True if constructor has recursive argument
typeCheckConstructor :: Name -> Type -> Sized -> Co -> [Pol] -> Telescope -> Constructor -> TypeCheck (Bool, Kinded EConstructor)
typeCheckConstructor d dt sz co pos dtel (Constructor n mctel t) = enter ("constructor " ++ show n) $ do
  let tel = maybe dtel fst mctel
{-
  tel <- case cpars of
    -- old style data parameters
    Nothing -> return dtel
    -- new style pattern parameters
    Just{}  -> computeConstructorTele dtel dt t
-}
  sig <- gets signature
  let telE = setDec irrelevantDec tel -- need kinded tel!!
    -- parameters are erased in types of constructors
  let tt = teleToType telE t
  echoTySig n tt
  let params = size tel
  -- when checking constructor types,  do NOT resurrect telescope
  --   data T [A : Set] : Set { inn : A -> T A }
  -- should be rejected, since A ~= T A, and T A = T B means A ~=B for arb. A, B!
  -- add data name as spos var, to check positivity
  -- and as NoKind, to compute the true kind from the constructors
  let telWithD = Telescope $ (TBind d $ Domain dt NoKind $ Dec SPos) : telescope tel
  Kinded ki te <- addBinds telWithD $
    checkConType sz t -- do NOT resurrect telescope!!

  -- Check target of constructor.
  dv <- whnf' dt
  let (Telescope argts,target) = typeToTele te
  whenNothing mctel $ -- only for old-style parameters
    addBinds telWithD $ addBinds (Telescope argts) $ checkTarget d dv tel target

  -- Make type of a constructor a singleton type.
  let mkName i n | emptyName n = fresh $ "y" ++ show i
                 | otherwise   = n
      fields = map boundName argts
      argns  = zipWith mkName [0..] $ fields
      argtbs = zipWith (\ n tb -> tb { boundName = n }) argns argts
--      core   = (foldl App (con (coToConK co) n) $ map Var argns)
      core   = Record (NamedRec (coToConK co) n False notDotted) $ zip fields $ map Var argns
      tsing  = teleToType (Telescope argtbs) $ Sing core target

  let tte = teleToType telE tsing -- te -- DO resurrect here!
  vt <- whnf' tte

  -- Now, compute the remaining information concerning the constructor.

  {- old code was more accurate, since it evaluated before checking
     for recursive occurrence.
  recOccs <- sposConstructor d 0 pos vt -- get recursive occurrences
  -}
  mutualNames <- asks mutualNames
  let mutOcc tb = not $ null $ List.intersect (d:mutualNames) $ usedDefs $ boundType tb
      recOccs   = map mutOcc argts
      isRec     = or recOccs
  -- fType <- extractType vt -- moved to Extract
  let fType = undefinedFType n
  isSz <- if sz /= Sized then return Nothing else do
    szConstructor d co params vt -- check correct use of sizes
    if co == CoInd then return $ Just $ error "impossible lhs type of coconstructor" else do
    let (x, lte) = mapSnd (teleToType telE) $ mkConLType params te
    echoKindedTySig kTerm n lte
    ltv <- whnf' lte
    return $ Just (x, ltv)

  -- Add the type constructor to the signature.
  let cpars = fmap (mapFst (map boundName . telescope)) mctel -- deletes types, keeps names
  addSigQ n (ConSig cpars isSz recOccs vt d (size dtel) fType)
--  let (tele, te) = typeToTele (length tel) tte -- NOT NECESSARY
  echoKindedTySig kTerm n tte
  -- traceM ("kind of " ++ n ++ "'s args: " ++ show ki)
--  echoTySigE n tte
  return (isRec, Kinded ki $ Constructor n (fmap (mapFst (const telE)) mctel) te)

typeCheckMeasuredFuns :: Co -> [Fun] -> TypeCheck [EFun]
typeCheckMeasuredFuns co funs0 = do
    -- echo $ show funs
    kfse <- mapM typeCheckFunSig funs0 -- NO LONGER erases measure
    -- use erased type signatures with retaines measure
    let funs = zipWith (\ (Kinded ki ts) f -> f { funTypeSig = ts }) kfse funs0

    -- type check and solve size constraints
    -- return clauses with meta vars resolved
    kcle <- installFuns co (zipWith Kinded (map kindOf kfse) funs) $
      mapM typeCheckFunClauses funs
    let kis  = map kindOf kcle
    let clse = map valueOf kcle
{-
    -- replace old clauses by new ones in funs
    let funs' = zipWith (\(tysig,(ar,cls)) cls' -> (tysig,(ar,cls'))) funs clss
-}
    -- get the list of mutually defined function names
    let funse = List.zipWith5 Fun
                  (map (fmap eraseMeasure . valueOf) kfse)
                  (map funExtName funs)
                  (map funArity funs)
                  (map funUnfolds funs)
                  clse
    -- print reconstructed clauses
    mapM_ (\ (Fun (TypeSig n t) _n' _ar _unfolds cls) -> do
        -- echoR $ n ++ " : " ++ show t
        echoR $ (PP.render $ prettyFun n cls))
      funse
    -- replace in signature by erased clauses
    zipWithM_ (enableSig co) (zipWith intersectKind kis $ map kindOf kfse) funse
    return $ funse

  where
    enableSig :: Co -> Kind -> Fun -> TypeCheck ()
    enableSig co ki (Fun (TypeSig n t) n' ar unfolds cl) = do
      vt <- whnf' t
      addSig n (FunSig co vt ki ar unfolds cl True $ undefinedFType $ QName n)
      -- add a let binding for external use
      v <- up False (vFun n) vt
      addSig n' (LetSig vt ki v [n] $ undefinedFType $ QName n')



-- type check the body of one function in a mutual block
-- type signature is already checked and added to local context
typeCheckFunBody :: Co -> Kind -> Fun -> TypeCheck EFun
typeCheckFunBody co ki0 fun@(Fun ts@(TypeSig n t) n' ar unfolds cls0) = do
    -- echo $ show fun
    addFunSig co $ Kinded ki0 fun
    -- type check and solve size constraints
    -- return clauses with meta vars resolved
    Kinded ki clse <- setCo co $ addUnfolds unfolds $ typeCheckFunClauses fun

    -- check new clauses for admissibility, inserting "unusuable" flags in the patterns where necessary
    -- TODO: proper cleanup, proper removal of admissibility check!
    -- clse <- admCheckFunSig co names ts clse

    -- print reconstructed clauses
    -- echoR $ n ++ " : " ++ show t
    echoR $ (PP.render $ prettyFun n clse)
    -- replace in signature by erased clauses
    let fune = Fun ts n' ar unfolds clse
    enableSig ki fune
    return fune


typeCheckFuns :: Co -> [Fun] -> TypeCheck [EFun]
typeCheckFuns co funs0 = do
    -- echo $ show funs
    kfse <- mapM typeCheckFunSig funs0
    let kfuns = zipWith (\ (Kinded ki ts) (Fun ts0 n' ar unfolds cls) -> Kinded ki (Fun ts n' ar unfolds cls)) kfse funs0
    -- zipWithM (addFunSig co) (map kindOf kfse) funs
    mapM_ (addFunSig co) kfuns
    let funs = map valueOf kfuns
    -- type check and solve size constraints
    -- return clauses with meta vars resolved
    kce <- setCo co $ mapM typeCheckFunClauses funs
    let kis = map kindOf kce
    let clse = map valueOf kce
    -- get the list of mutually defined function names
    let names   = map (\ (Fun (TypeSig n _t) _n _ar _unfolds _cls) -> n) funs
    -- check new clauses for admissibility, inserting "unusuable" flags in the patterns where necessary
    -- TODO: proper cleanup, proper removal of admissibility check!
    clse <- zipWithM (\ (Fun tysig _ _ _ _) cls' -> admCheckFunSig co names tysig cls') funs clse
    -- replace old clauses by new ones in funs
    let funse = List.zipWith5 Fun
                  (map valueOf kfse)
                  (map funExtName funs)
                  (map funArity funs)
                  (map funUnfolds funs)
                  clse
--    let funse = zipWith (\(tysig,(ar,cls)) cls' -> (tysig,(ar,cls'))) funs clse
    -- print reconstructed clauses
    mapM_ (\ (Fun (TypeSig n t) _n' _ar _unfolds cls) -> do
        -- echoR $ n ++ " : " ++ show t
        echoR $ (PP.render $ prettyFun n cls))
      funse
    terminationCheck funse
    -- replace in signature by erased clauses
    zipWithM_ enableSig kis funse
    return $ funse

addFunSig :: Co -> Kinded Fun -> TypeCheck ()
addFunSig co (Kinded ki (Fun (TypeSig n t) n' ar unfolds cl)) = do
    sig <- gets signature
    vt <- whnf' t -- TODO: PROBLEM for internal extraction (would need te here)
    addSig n (FunSig co vt ki ar unfolds cl False $ undefinedFType $ QName n) --not yet type checked / termination checked

-- ADMCHECK FOR COFUN is not taking place in checking the lhs
-- TODO: proper analysis for size patterns!
-- admCheckFunSig mutualNames (TypeSig thisName thisType, clauses)
admCheckFunSig :: Co -> [Name] -> TypeSig -> [Clause] -> TypeCheck [Clause]
admCheckFunSig CoInd  mutualNames (TypeSig n t) cls = return cls
admCheckFunSig co@Ind mutualNames (TypeSig n t) cls = traceAdm ("admCheckFunSig: checking admissibility of " ++ show n ++ " : " ++ show t) $
   (
    do -- a function is not recursive if did does not mention any of the
       -- mutually defined function names
       let usedNames = rhsDefs cls
       let notRecursive = all (\ n -> not (n `elem` usedNames)) mutualNames
       -- for non-recursive functions, we can skip the admissibility check
       if notRecursive then
          -- trace ("function " ++ n ++ " is not recursive") $
            return cls
        else -- trace ("function " ++ n ++ " is recursive ") $
          do vt <- whnf' t
             admFunDef co cls vt
    ) `throwTrace` ("checking type of " ++ show n ++ " for admissibility")


enableSig :: Kind -> Fun -> TypeCheck ()
enableSig ki (Fun (TypeSig n _) n' ar' unfolds' cl') = do
  (FunSig co vt ki0 ar unfolds cl _ ftyp) <- lookupSymb n
  addSig n (FunSig co vt (intersectKind ki ki0) ar unfolds cl' True ftyp)
  -- add a let binding for external use
  v <- up False (vFun n) vt
  addSig n' (LetSig vt ki v [n] ftyp)


-- typeCheckFunSig (TypeSig thisName thisType, clauses)
typeCheckFunSig :: Fun -> TypeCheck (Kinded ETypeSig)
typeCheckFunSig (Fun (TypeSig n t) _n _ar _unfolds _cls) = enter ("type of " ++ show n) $ do
  echoTySig n t
  Kinded ki0 te <- checkType t
  -- let te = eraseMeasure te0
  let ki = predKind ki0
  echoKindedTySig ki n (eraseMeasure te)
--  echoTySigE n te
  return $ Kinded ki $ TypeSig n te

typeCheckFunClauses :: Fun -> TypeCheck (Kinded [EClause])
typeCheckFunClauses (Fun (TypeSig n t) _n' _ar unfolds cl) = enter (show n) $
   do result@(Kinded _ cle) <- addUnfolds unfolds $ checkFun t cl
      -- traceCheck (show (TypeSig n t)) $
       -- traceCheck (show cl') $
      -- echo $ PP.render $ prettyFun n cle
      return result

-- checkConType sz t = Kinded ki te
-- the returned kind is the kind of the constructor arguments
-- check that result is a universe
--  ( params were already checked by checkDataType and are not included in t )
-- called initially in the context consisting of the parameter telescope
checkConType :: Sized -> Expr -> TypeCheck (Kinded Extr)
checkConType NotSized t = checkConType' t
checkConType Sized t =
    case t of
      Quant Pi tb@(TBind _ (Domain t1 _ _)) t2 | isSize t1 -> do
             addBind (mapDec (const paramDec) tb) $ do  -- size is parametric in constructor type
               Kinded ki t2e <- checkConType' t2
               return $ Kinded ki $ Quant Pi (mapDec (const irrelevantDec) tb) t2e -- size is irrelevant in constructor
      _ -> throwErrorMsg $ "checkConType: expecting size quantification, found " ++ show t

checkConType' :: Expr -> TypeCheck (Kinded Extr)
checkConType' t = do
  (s, kte) <- checkingCon True $ inferType t
  case s of
    Set{} -> return kte
    CoSet{} -> return kte
    _ -> throwErrorMsg $ "checkConType: type " ++ show t ++ " of constructor not a universe"

-- check that the data type and the parameter arguments (written down like declared in telescope)
-- precondition: target tg type checks in current context
checkTarget :: Name -> TVal -> Telescope -> Type -> TypeCheck ()
checkTarget d dv tel tg = do
  tv <- whnf' tg
  case tv of
    VApp (VDef (DefId DatK (QName n))) vs | n == d -> do
      telvs <- mapM (\ tb -> whnf' (Var (boundName tb))) $ telescope tel
      _ <- enter ("checking datatype parameters in constructor target") $
        leqVals' N mixed (One dv) (take (size tel) vs) telvs
      return ()
    _ -> throwErrorMsg $ "constructor should produce something in data type " ++ show d

-- check that params are types
-- check that arguments are stypes
-- check that target is set
checkDataType :: Int -> Expr -> TypeCheck (Kinded (Sort Expr, Extr))
checkDataType p e = do
  traceCheck ("checkDataType " ++ show e ++ " p=" ++ show p) $ do
  case e of
     Unfold ns e -> addUnfolds ns $ checkDataType p e
     Quant Pi tb@(TBind x (Domain t1 _ dec)) t2 -> do
       k <- getLen
       traceCheckM ("length of context = " ++ show k)
       -- t1e <- checkingDom $ if k <= p then checkType t1 else checkSmallType t1
       (s1, Kinded ki0 t1e) <- checkingDom $ inferType t1
       let ki1 = predKind ki0
       addBind (TBind x (Domain t1 ki1 defaultDec)) $ do
         Kinded ki2 (s, t2e) <- checkDataType p t2
         -- when k <= p $ ltSort s1 s -- check size of indices (disabled)
         return $ Kinded ki2 (s, Quant Pi (TBind x (Domain t1e ki1 dec)) t2e)
     Sort s@(Set e1)   -> do
       (_, e1e) <- checkLevel e1
       return $ Kinded (kUniv e1e) (s, Sort $ Set e1e)
     Sort s@(CoSet e1) -> do
       e1e <- checkSize e1
       return $ Kinded (kUniv Zero) (s, Sort $ CoSet e1e)
     _ -> throwErrorMsg "doesn't target Set or CoSet"

{-
checkSize :: Expr -> TypeCheck Extr
checkSize Infty = return Infty
checkSize e = valueOf <$> checkExpr e vSize
-}

checkSize :: Expr -> TypeCheck Extr
checkSize e =
  case e of
    Meta i  -> do
      ren <- asks renaming
      addMeta ren i
      return e
    e       -> inferSize e

inferSize :: Expr -> TypeCheck Extr
inferSize e =
  case e of
    Zero  -> return e
    Infty -> return e
    Succ e  -> Succ <$> checkSize e
    Plus es -> Plus <$> mapM checkSize es
    Max  es -> maxE <$> mapM checkSize es
    e -> do
      (v, Kinded ki e) <- inferExpr e
      subtype v vSize
      return e

checkBelow :: Expr -> LtLe -> Val -> TypeCheck Extr
checkBelow e Le VInfty = checkSize e
checkBelow e ltle v = do
  e' <- checkSize e
  v' <- whnf' e
  leSize ltle Pos v' v
  return e'


-- checkLevel e = (value of e, ee)
-- if e : Size and value of e != Infty
checkLevel :: Expr -> TypeCheck (Val, Extr)
checkLevel e = do
  Kinded _ ee <- checkExpr e vSize
  v  <- whnf' e
  when (v == VInfty) $ recoverFail $ "# is not a valid universe level"
  return (v, ee)

{- Kind inference

          i    : Size              : Type
      t : Nat  : Set  : Set1 : ... : Type = Set\omega
  p : P : Prop : Set  : ...

Functional, cumulative PTS (s,s',s') written (s,s')

  (Size,s)   s != Size    size-dependency
  (s,Prop)                impredicative Prop
  (Set_i,Set_j)  i <= j   predicativity

Kind    can be used to construct Kinds
term t  terms, types, universes, proofs, propositions
type T  types, universes, propositions
size i  types, universes, propositions
prf  p  proofs
pred P  types, universes, propositions

We like to infer kinds of expressions

  Tm < Set < Set1 < Set2 < ...

For  t : A  if kind(A) = Tm then t is a term,
                       = Set then t is a type,
                       = Set1 then t is a type1 (e.g, a universe) ...

Then,  if  t : (x : A) -> B
      and  kind(A) `irrelevantFor` kind(B)   [ with irrelevantFor := > ]

we can change the type signature to

           t : [x : A] -> B

This is because you cannot eliminate a type to produce a term.

  kind(Set)  = Set
  kind(Size) = Size -- this means that we treat sizes as types, they cannot
  kind(s)    = s    -- if s is a sort
  kind((x : A) -> B) = kind(B)
  kind(A : Set0) = Tm
  kind(A : Prop) = Prf
  kind(A : Size) = <<impossible>>
  kind(A : Setk) = k-1

irrFor Tm  _    = False
irrFor Ty Tm    = True
irrFor Ty Prf   = True
irrFor Ty _     = False
irrFor Size Tm  = True
irrFor Size Prf = True

One problem is that we cannot infer exact kinds, e.g.

  fun T : Bool -> Set 1 -- T is a type
  { T true  = Bool      -- T true is a type
  ; T false = Set 0     -- T false is a universe
  }

T is either a type or a universe.  So we can only assign intervals.
This is like in Augustsson's Cayenne [not in his paper, though].

A datatype is always a type.  A size is a type.
A constructor is always a term.

-}


-- type checking

-- checkExpr e tv = (e', ki)
-- e' is the version of e with erasure marker at irrelevant positions
-- ki is the kind of e (Tm, Ty, Set ...)
-- ki is at most the predecessor of the sort of tv
--
-- this is *internal* extraction in the style of Barras and Bernardo
-- e.g., does not prune t : Id A a b
-- thus, we can use the pruned version for evaluation!
checkExpr :: Expr -> TVal -> TypeCheck (Kinded Extr)
checkExpr e v = do
  l <- getLen
  enterDoc (text ("checkExpr " ++ show l ++ " |-") <+> prettyTCM e <+> colon <+> prettyTCM v) $ do

   ce <- ask
   traceCheck ("checkExpr: " ++ show (renaming ce) ++ ";" ++ show (context ce) ++ " |- " ++ show e ++ " : " ++ show v ++ " in env" ++ show (environ ce)) $ do

    (case (e, v) of

{- In the presence of full bracket types,
   we could implement the following "resurrecting version of let"

   Gamma |- s : [A]
   Gamma, x:A |- t : C   Gamma, x:A, y:A |- t = t[y/x] : C
   -------------------------------------------------------
   Gamma |- let x:[A] = s in t : C

 -}

      (App (Lam dec x f) e, v) | inferable e -> checkLet dec x emptyTel Nothing e f v

{-
      (LLet (TBind x (Domain Nothing _ dec)) e1 e2, v) -> checkUntypedLet x dec e1 e2 v
      (LLet (TBind x (Domain (Just t1) _ dec)) e1 e2, v) -> checkTypedLet x t1 dec e1 e2 v
-}
      (LLet (TBind x (Domain mt _ dec)) tel e1 e2, v) -> checkLet dec x tel mt e1 e2 v

      (Case (Var x) Nothing [Clause _ [SuccP (VarP y)] (Just rhs)], v) -> do
          (tv, _) <- resurrect $ inferExpr (Var x)
          subtype tv vSize
          vx@(VGen i) <- whnf' (Var x)
          endsInSizedCo i v
          let dom = Domain vSize kSize defaultDec
          newWithGen y dom $ \ j vy -> do
            let vp = VSucc vy
            addSizeRel j 1 i $
              addRewrite (Rewrite vx vp) [v] $ \ [v'] -> do
                Kinded ki2 rhse <- checkRHS emptySub rhs v'
                return $ Kinded ki2 $ Case (Var x) (Just tSize) [Clause [TBind y dom] [SuccP (VarP y)] (Just rhse)]


      (Case e mt cs, v) -> do
          (tv, t, Kinded ki1 ee) <- checkOrInfer neutralDec e mt
          ve <- whnf' ee
          -- tv' <- sing' ee tv -- DOES NOT WORK
          Kinded ki2 cle <- checkCases ve (arrow tv v) cs
          return $ Kinded ki2 $ Case ee (Just t) cle
{-
      (Case e Nothing cs, _) -> do
          (tv, Kinded ki1 ee) <- inferExpr e
          ve <- whnf' ee
          -- tv' <- sing' ee tv -- DOES NOT WORK
          Kinded ki2 cle <- checkCases ve (arrow tv v) cs
          t <- toExpr tv
          return $ Kinded ki2 $ Case ee (Just t) cle
-}
      (_, VGuard beta bv) ->
        addBoundHyp beta $ checkExpr e bv

      (e, VUnfold xs v) ->
        addUnfolds xs $ checkExpr e v

      (e,v) | inferable e -> do
          (v2, Kinded ki1 ee) <- inferExpr e
          checkSubtype ee v2 v
          return $ Kinded ki1 ee

      _ -> checkForced e v

      ) -- >> (trace ("checkExpr successful: " ++ show e ++ ":" ++ show v) $ return ())

-- | checkLet @let .x tel : t = e1 in e2@
checkLet :: Dec -> Name -> Telescope -> Maybe Type -> Expr -> Expr -> TVal -> TypeCheck (Kinded Extr)
checkLet dec x tel mt1 e1 e2 v = do
  (v_t1, t1e, Kinded ki1 e1e) <- checkLetDef dec tel mt1 [] e1
--  (v_t1, t1e, Kinded ki1 e1e) <- checkOrInfer dec e1 mt1
  checkLetBody x t1e v_t1 ki1 dec e1e e2 v

-- | checkLetDef @.x tel : t = e@ becomes @.x : tel -> t = \ tel -> e@
checkLetDef :: Dec -> Telescope -> Maybe Type -> Unfolds -> Expr -> TypeCheck (TVal, EType, Kinded Extr)
checkLetDef dec tel mt unf e = local (\ cxt -> cxt {consistencyCheck = True}) $ do
  -- 2013-04-01
  -- since a let telescope is treated like a lambda abstraction
  -- and the let-defined symbol reduces by itself, we need to
  -- do the context consistency check at each introduction.
  (tel, (vt, te, Kinded ki ee)) <- checkTele tel $ addUnfolds unf $ checkOrInfer dec e mt
  te <- return $ teleToType tel te
  ee <- return $ teleLam tel ee
  vt <- whnf' te
  return (vt, te, Kinded ki ee)

{-
checkTypedLet :: Name -> Type -> Dec -> Expr -> Expr -> TVal -> TypeCheck (Kinded Extr)
checkTypedLet x t1 dec e1 e2 v = do
  Kinded kit t1e <- checkType t1
  v_t1 <- whnf' t1
  Kinded ki0 e1e <- applyDec dec $ checkExpr e1 v_t1
  let ki1 = intersectKind ki0 (predKind kit)
  checkLetBody x t1e v_t1 ki1 dec e1e e2 v
{-
  v_e1 <- whnf' e1
  new x (Domain v_t1 ki1 dec) $ \ vx -> do
    addRewrite (Rewrite vx v_e1) [v] $ \ [v'] -> do
      Kinded ki2 e2e <- checkExpr e2 v'
      return $ Kinded ki2 $ LLet (TBind x (Domain t1e ki1 dec)) e1e e2e  -- if e2e==Irr then Irr else LLet n t1e e1e e2e
-}

checkUntypedLet :: Name -> Dec -> Expr -> Expr -> TVal -> TypeCheck (Kinded Extr)
checkUntypedLet x dec e1 e2 v = do
  (v_t1, Kinded ki1 e1e) <- applyDec dec $ inferExpr e1
  v_e1 <- whnf' e1
  t1e <- toExpr v_t1
  checkLetBody x t1e v_t1 ki1 dec e1e e2 v
-}

checkLetBody :: Name -> EType -> TVal -> Kind -> Dec -> Extr -> Expr -> TVal -> TypeCheck (Kinded Extr)
checkLetBody x t1e v_t1 ki1 dec e1e e2 v = do
  v_e1 <- whnf' e1e
  new x (Domain v_t1 ki1 dec) $ \ vx -> do
    addRewrite (Rewrite vx v_e1) [v] $ \ [v'] -> do
      Kinded ki2 e2e <- checkExpr e2 v'
      return $ Kinded ki2 $ LLet (TBind x (Domain (Just t1e) ki1 dec)) emptyTel e1e e2e
{-
-- Dependent let: not checkable in rho;Delta style
--            v_e1 <- whnf rho e1
--            checkExpr (update rho n v_e1) (v_t1 : delta) e2 v
-}

-- | @checkPair e1 e2 y dom env b@ checks @Pair e1 e2@ against
--   @VQuant Sigma y dom env b@.
checkPair :: Expr -> Expr -> Name -> Domain -> FVal -> TypeCheck (Kinded Expr)
checkPair e1 e2 y dom@(Domain av ki dec) fv = do
  case av of
    VBelow Lt VInfty -> do
      lowerSemi <- underAbs y dom fv $ \ i _ bv -> lowerSemiCont i bv
      continue $ if lowerSemi then VBelow Le VInfty else av
    _ -> continue av
  where
    continue av = do
      Kinded k1 e1 <- applyDec dec $ checkExpr e1 av
      v1 <- whnf' e1
      bv <- app fv v1
      Kinded k2 e2 <- checkExpr e2 bv
      return $ Kinded (unionKind k1 k2) $ Pair (maybeErase dec e1) e2

-- check expression after forcing the type
checkForced :: Expr -> TVal -> TypeCheck (Kinded Expr)
checkForced e v = do
  ren <- asks renaming
  v   <- force v
--  enter ("checkForced " ++ show ren ++ " |- " ++ show e ++ " : " ++ show v) $ do
  enterDoc (text ("checkForced " ++ show ren ++ " |-") <+> prettyTCM e <+> colon <+> prettyTCM v) $ do
    case (e,v) of
{-
      (_, VGuard (Bound (Measure [VGen i]) (Measure [VGen j])) bv) ->
        addSizeRel i j $ checkForced e bv
-}
      (_, VGuard beta bv) ->
        addBoundHyp beta $ checkForced e bv

      (e, VUnfold xs v) ->
        addUnfolds xs $ checkForced e v

      (Unfold xs e, v) ->
        addUnfolds xs $ checkForced e v

      (Pair e1 e2, VQuant Sigma y dom@(Domain av ki dec) fv) ->
        checkPair e1 e2 y dom fv

      (Record ri rs, t@(VApp (VDef (DefId DatK d)) vl)) -> do
         let fail1 = failDoc (text "expected" <+> prettyTCM t <+> text "to be a record type")
--         DataSig { numPars, isTuple } <- lookupSymb d
--         unless isTuple $ fail1
         mfs <- getFieldsAtType d vl
         case mfs of
           Nothing -> fail1
           Just ptv -> do
             let checkField :: (Name, Expr) -> TypeCheck (Kinded [(Name,Expr)]) -> TypeCheck (Kinded [(Name,Expr)])
                 checkField (p,e) cont =
                  case lookup p ptv of
                    Nothing -> failDoc (prettyTCM p <+> text "is not a field of record" <+> prettyTCM t)
                    Just tv -> do
                      tv <- piApp tv VIrr -- remove record argument (cannot be dependent!)
                      Kinded k e <- checkExpr e tv
                      Kinded k' es <- cont
                      return $ Kinded (unionKind k k') ((p,e) : es)
             Kinded k rs <- foldr checkField (return $ Kinded NoKind []) rs
             return $ Kinded k $ Record ri rs


{- OLD:
Following Awodey/Bauer 2001, the following rule is valid

   Gamma, x:A |- t : B    Gamma, x:A, y:A |- t = t[y/x] : B
   --------------------------------------------------------
   Gamma |- \xt : Pi x:[A]. B

      (Lam _ y e1, VPi dec x va env t1) -> do
          rho <- getEnv  -- get the environment corresponding to Gamma
          new y (Domain va (resurrectDec dec)) $ \ vy -> do
            v_t1 <- whnf (update env x vy) t1
            -- traceCheckM $ "checking " ++ show e1 ++ " : " ++ show v_t1
            e1e <- checkExpr e1 v_t1
            when (erased dec) $ do  -- now check invariance of the e1
              new y (Domain va (resurrectDec dec)) $ \ vy' -> do
                ve  <- whnf (update rho y vy)  e1e
                ve' <- whnf (update rho y vy') e1e
                eqVal v_t1 ve ve'  -- BUT: ve' does not have type v_t1 !?
            -- prune the lambda if body has been pruned
            return $ if e1e==Irr then Irr else Lam y e1e
 -}

-- NOW just my rule (LICS 2010 draft) a la Barras/Bernardo

      (Lam _ y e1, VQuant Pi x dom fv) -> do
          -- rho <- getEnv  -- get the environment corresponding to Gamma
          underAbs y dom fv $ \ _ vy bv -> do
            -- traceCheckM $ "checking " ++ show e1 ++ " : " ++ show v_t1
            Kinded ki1 e1e <- checkExpr e1 bv
            -- the kind of a lambda is the kind of its body
            return $ Kinded ki1 $ Lam (decor dom) y e1e

      -- lone projection: eta-expand!
      (Proj Pre p, VQuant Pi x dom fv) -> do
         let y = nonEmptyName x "y"
         checkForced (Lam (decor dom) y $ App e (Var y)) v
{-
      -- should be subsumed by checkBelow:
      (e, v) | isVSize v -> Kinded kSize <$> checkSize e
-}
{-  MOVED to checkSize

      -- metavariables must have type size
      (Meta i, _) | isVSize v -> do
        addMeta ren i
        return $ Kinded kSize $ Meta i

     (Infty, v) | isVSize v -> return $ Kinded kSize $ Infty
      (Zero, v) | isVSize v -> return $ Kinded kSize $ Zero

      (Plus es, v) | isVSize v -> do
              ese <- mapM checkSize es
              return $ Kinded kSize $ Plus ese

      (Max es, v) | isVSize v -> do
              ese <- mapM checkSize es
              return $ Kinded kSize $ Max ese

      (Succ e2, v) | isVSize v -> do
              e2e <- checkSize e2
              return $ Kinded kSize $ Succ e2e
-}

      (e, VBelow ltle v) -> Kinded kSize <$> checkBelow e ltle v
{-
              -- prune sizes
              return $ if e2e==Irr then Irr else Succ e2e
-}
      (e,v) -> do
        case spineView e of

          -- unfold defined patterns
          (h@(Def (DefId (ConK DefPat) c)), es) -> do
             PatSig xs pat _ <- lookupSymbQ c
             let (xs1, xs2) = splitAt (length es) xs
                 phi x      = maybe (Var x) id $ lookup x (zip xs1 es)
                 body       = parSubst phi (patternToExpr pat)
                 e          = foldr (Lam defaultDec) body xs2
             checkForced e v

          -- check constructor term
          (h@(Def (DefId (ConK co) c)), es) -> checkConTerm co c es v
{-
          (h@(Def (DefId (ConK co) c)), es) -> do
             tv <- conType c v
             (knes, dv) <- checkSpine es tv
             let e = foldl App h $ map (snd . valueOf) knes
             checkSubtype e dv v
             e <- etaExpandPis e dv -- a bit similiar to checkSubtype, which computes a singleton
             return $ Kinded kTerm $ e
-}
          -- else infer
          _ -> do
            (v2,kee) <- inferExpr e
            checkSubtype (valueOf kee) v2 v
            return kee

-- | Check (partially applied) constructor term, eta-expand it and turn it
--   into a named record.
checkConTerm :: ConK -> QName -> [Expr] -> TVal -> TypeCheck (Kinded Extr)
checkConTerm co c es v = do
  case v of
    VQuant Pi x dom fv -> do
      let y = freshen $ nonEmptyName x "y"
      underAbs y dom fv $ \ _ _ bv -> do
        Kinded ki ee <- checkConTerm co c (es ++ [Var y]) bv
        return $ Kinded ki $ Lam (decor dom) y ee
    _ -> do
      c <- disambigCon c v
      tv <- conType c v
      (knes, dv) <- checkSpine es tv
      let ee = Record (NamedRec co c False notDotted) $ map valueOf knes
      checkSubtype ee dv v
      return $ Kinded kTerm ee

{-
-- | Check (partially applied) constructor term, eta-expand it and turn it
--   into a named record.
checkConTerm :: ConK -> Name -> [Expr] -> TVal -> TypeCheck (Kinded Extr)
checkConTerm co c es v = do
  tv <- conType c v
  (knes, dv) <- checkSpine es tv
  let e0 = foldl App (Def (DefId (ConK co) c)) $ map (snd . valueOf) knes
  checkSubtype e0 dv v
  (vTel, _) <- telView dv
  let xs   = map (boundName . snd) vTel
      decs = map (decor . boundDom . snd) vTel
      ys   = map freshen xs
      rs   = map valueOf knes ++ (zip xs $ map Var ys)
      e1   = Record (NamedRec co c False) rs
      e    = foldr (uncurry Lam) e1 (zip decs ys)
  return $ Kinded kTerm e
-}

{-
-- | Only eta-expand at function types, do not force.
etaExpandPis :: Expr -> TVal -> TypeCheck Expr
etaExpandPis e tv = do
  case tv of
    VQuant Pi x dom env b -> new x dom $ \ xv -> do
      let y = freshen x
      Lam (decor dom) y <$> do
        etaExpandPis (App e (Var y)) =<< whnf (update env x xv) b
    _ -> return e
-}

checkSpine :: [Expr] -> TVal -> TypeCheck ([Kinded (Name, Extr)], TVal)
checkSpine [] tv = return ([], tv)
checkSpine (e : es) tv = do
  (kne, tv) <- checkApp e tv
  (knes, tv) <- checkSpine es tv
  return (kne : knes, tv)

maybeErase :: Polarity pol => pol -> Expr -> Expr
maybeErase dec = if erased dec then erasedExpr else id

-- | checking e against (x : A) -> B returns (x,e) and B[e/x]
checkApp :: Expr -> TVal -> TypeCheck (Kinded (Name, Extr), TVal)
checkApp e2 v = do
  v <- force v -- if v is a corecursively defined type in Set, unfold!
  enter ("checkApp " ++ show v ++ " eliminated by " ++ show e2) $ do
  case v of
    VQuant Pi x dom@(Domain av@(VBelow Lt VInfty) _ dec) fv -> do
      upperSemi <- underAbs x dom fv $ \ i _ bv -> upperSemiCont i bv
      continue $ if upperSemi then VQuant Pi x dom{ typ = VBelow Le VInfty} fv
                 else v
    _ -> continue v
 where
  continue v = case v of
    VQuant Pi x (Domain av _ dec) fv -> do
       (ki, v2, e2e) <- do
         if inferable e2 then do
       -- if e2 has a singleton type, we should not take v2 = whnf e2
       -- but use the single value of e2
       -- this is against the spirit of bidir. checking
              -- if checking a type we need to resurrect
              (av', Kinded ki e2e) <- applyDec dec $ inferExpr e2
              case av' of
                 VSing v2 av'' -> do subtype av' av
                                     return (ki, v2, e2e)
                 _ -> do checkSubtype e2e av' av
                         v2 <- whnf' e2e
                         return (ki, v2, e2e)
            else do
              Kinded ki e2e <- applyDec dec $ checkExpr e2 av
              v2 <- whnf' e2e
              return (ki, v2, e2e)
       bv <- app fv v2
       -- the kind of the application is the kind of its head
       return (Kinded ki $ (x,) $ maybeErase dec e2e, bv)
       -- if e1e==Irr then Irr else if e2e==Irr then e1e else App e1e [e2e])
    _ -> throwErrorMsg $ "checking application to " ++ show e2 ++ ": expected function type, found " ++ show v


-- checkSubtype  expr : infered_type <= ascribed_type
checkSubtype :: Expr -> TVal -> TVal -> TypeCheck ()
checkSubtype e v2 v = do
    rho <- getEnv
    traceSingM $ "computing singleton <" ++ show e ++ " : " ++ show v2 ++ ">" -- ++ " in environment " ++ show rho
    v2principal <- sing rho e v2
    traceSingM $ "subtype checking " ++ show v2principal ++ " ?<= " ++ show v ++ " in environment " ++ show rho
    subtype v2principal v


-- ptsRule s1 s2 = s  if (s1,s2,s) is a valid rule
-- precondition: s1,s2 are proper sorts, i.e., not Size or Tm
ptsRule :: Bool -> Sort Val -> Sort Val -> TypeCheck (Sort Val)
ptsRule er s1 s2 = do
  cxt <- ask
  let parametric = checkingConType cxt  -- are we dealing with a parametric pi?
  let err = "ptsRule " ++ show (s1,s2) ++ " " ++ (if parametric then "(in type of constructor)" else "") ++ ": "
  case (s1,s2) of
    (Set VInfty,_) -> throwErrorMsg $ err ++ "domain too big"
    (Set v1, Set v2) ->
      if parametric then do
         unless er $ leqSize Pos v1 v2 -- when we are checking a constructor, to reject
         {- data Bad : Set { bad : Set -> Bad } -}
         return s2
       else return $ Set $ maxSize [v1,v2]
    (CoSet v1, Set VZero)
       | parametric   -> return $ CoSet v1
       | v1 == VInfty -> return $ Set VZero
       | otherwise    -> throwErrorMsg $ err ++ "domain cannot be sized"
    (CoSet v1, CoSet v2)
       | parametric   -> do
           let v2' = maybe v2 id $ predSize v2
           case minSize v1 v2 of
             Just v  -> return $ CoSet v
             Nothing -> throwErrorMsg $ err ++ "min" ++ show (v1,v2) ++ " does not exist"
       | v1 == VInfty -> return $ CoSet $ succSize v2
       | otherwise    -> throwErrorMsg $ err ++ "domain cannot be sized"
    _ -> return s2

checkOrInfer :: Dec -> Expr -> Maybe Type -> TypeCheck (TVal, EType, Kinded Extr)
checkOrInfer dec e Nothing = do
  (tv, ke) <- applyDec dec $ inferExpr e
  te <- toExpr tv
  return (tv, te, ke)
checkOrInfer dec e (Just t) = do
  Kinded kt te <- checkType t
  tv <- whnf' te
  Kinded ke ee <- applyDec dec $ checkExpr e tv
  let ki = intersectKind ke $ predKind kt
  return $ (tv, te, Kinded ki ee)

-- inferType t = (s, te)
inferType :: Expr -> TypeCheck (Sort Val, Kinded Extr)
inferType t = do
  (sv, te) <- inferExpr t
  -- force sv >>= \case
  case sv of
    VSort s | not (s `elem` map SortC [Tm,Size]) -> return (s,te)
    sv' -> failDoc $ vcat
      [ hsep [ "inferExpr: expected", prettyTCM t, "to be a type,"
             , "but got:", prettyTCM sv' ]
      , maybe PrettyTCM.empty (\ unf -> hsep [ "We can unfold:", prettyTCM unf ]) =<< askUnfoldables
      ]

-- inferExpr e = (tv, s, ee)
-- input : expr e | inferable e
-- output: type tv, kind s, and erased form ee of e
-- the kind tells whether e is a term, a size, a set, ...
inferExpr :: Expr -> TypeCheck (TVal, Kinded Extr)
inferExpr e = do
  (tv, ee) <- inferExpr' e
  case tv of
    VGuard beta vb -> do
      checkGuard beta
      return (vb, ee)
    _ -> return (tv, ee)

inferProj :: Expr -> PrePost -> Name -> TypeCheck (TVal, Kinded Extr)
inferProj e1 fx p = checkingCon False $ do
            (v, Kinded ki1 e1e) <- inferExpr e1
{-
            let fail1 = failDoc (text "expected" <+> prettyTCM e1 <+> text "to be of record type when taking the projection" <+> text p <> comma <+> text "but found type" <+> prettyTCM v)
            let fail2 = failDoc (text "record" <+> prettyTCM e1 <+> text "of type" <+> prettyTCM v <+> text "does not have field" <+> text p)
-}
            v <- force v -- if v is a corecursively defined type in Set, unfold!
            tv <- projectType v p =<< whnf' e1e
            return (tv, Kinded ki1 (proj e1e fx p))
{-
            case v of
              VApp (VDef (DefId Dat d)) vl -> do
                mfs <- getFieldsAtType d vl
                case mfs of
                  Nothing -> fail1
                  Just ptvs ->
                    case lookup p ptvs of
                      Nothing -> fail2
                      Just tv -> do
                        tv <- piApp tv VIrr -- cut of record arg
                        return (tv, Kinded ki1 (App e1e (Proj p)))
              _ -> fail1
-}


-- inferExpr' might return a VGuard, this is removed in inferExpr
-- the returned kind for constructor type is computed as the union
-- of the kinds of the non-erased arguments
-- otherwise it is the kind of the target
inferExpr' :: Expr -> TypeCheck (TVal, Kinded Extr)
inferExpr' e = enter ("inferExpr' " ++ show e) $
  let returnSing (Kinded ki ee) tv = do
        tv' <- sing' ee tv
        return (tv', Kinded ki ee)
  in
    (case e of

      -- Γ, [xs] ⊢ e ⇒ t
      -- ----------------------
      -- Γ ⊢ unfold xs in e ⇒ t

      Unfold xs e -> addUnfolds xs $ inferExpr' e

      Var x -> do
        traceCheckM ("infer variable " ++ show x)
        item <- lookupName1 x
        traceCheckM ("infer variable: retrieved item ")
        let dom = domain item
            av  = typ dom
        traceCheckM ("infer variable: " ++ show av)
        enterDoc (text "inferExpr: variable" <+> prettyTCM x <+> colon <+> prettyTCM av <+> text "may not occur") $ do
          let dec  = decor dom
              udec = upperDec item
              pol  = polarity dec
              upol = polarity udec
          when (erased dec && not (erased udec)) $
            recoverFail ", because it is marked as erased"
          enter ", because of polarity" $
            leqPolM pol upol
        traceCheckM ("infer variable returns")
        traceCheckM ("infer variable " ++ show x ++ " : " ++ show av)
        return $ (av, Kinded (kind dom) $ Var x)
{-
        let err = "inferExpr: variable " ++ x ++ " : " ++ show (typ item) ++
                  " may not occur"
        let dec = decor item
        let pol = polarity dec
        if erased dec then
          throwErrorMsg $ err ++ ", because it is marked as erased"
         else if not (leqPol pol SPos) then
          throwErrorMsg $ err ++ ", because it has polarity " ++ show pol
         else do
           -- traceCheckM ("infer variable " ++ x ++ " : " ++ show  (typ item))
           return $ (typ item, Var x) -- TODO: (typ item, kind item, Var x)
-}

      -- for constants, the kind coincides with the type!
      Sort (CoSet e) -> do
        ee <- checkSize e
        return (VSort (Set (VSucc VZero)), Kinded (kUniv Zero) $ Sort $ CoSet ee)
      Sort (Set e) ->  do
        (v, ee) <- checkLevel e
        return (VSort (Set (succSize v)), Kinded (kUniv ee) $ Sort $ Set ee)
      Sort (SortC Size) -> return (vTSize, Kinded kTSize $ e)
      Zero -> return (vSize, Kinded kSize Zero)
      Infty -> return (vSize, Kinded kSize Infty)
      Below ltle e -> do
        ee <- checkSize e
        return (vTSize, Kinded kTSize $ Below ltle ee)

      Quant pisig (TBind n (Domain t1 _ dec)) t2 -> do
        -- make sure that in a constructor declaration the constructor args are
        -- mixed-variant (there is no subtyping between constrs anyway)
        checkCon <- asks checkingConType
{- TODO
        when (checkCon && polarity dec /= Mixed) $
          throwErrorMsg $ "constructor arguments must be declared mixed-variant"
-}
        (s1, Kinded ki0 t1e) <- (if pisig==Pi then checkingDom else id) $
          checkingCon False $ inferType t1 -- switch off parametric Pi
        -- the kind of the bound variable is the precedessor of the kind of its type
        let ki1 = predKind ki0
        addBind (TBind n (Domain t1e ki1 $ defaultDec)) $ do -- ignore erasure flag AND polarity in Pi! (except for irrelevant, only becomes parametric)
        -- TODO:
        -- addBind (TBind n (Domain t1e ki1 $ coDomainDec dec)) $ do -- ignore erasure flag AND polarity in Pi! (except for irrelevant, only becomes parametric)
          (s2, Kinded ki2 t2e) <- inferType t2
          ce <- ask
          let er = erased dec
          s <- if impredicative ce && er && s2 == Set VZero then return s2 else ptsRule er s1 s2 -- Impredicativity!
          -- improve erasure annotation: irrelevant arguments can be erased!
          let (ki',dec') = if checkCon then
                 -- in case of constructor types the kind is the union
                 -- of the kinds of the constructor arguments
                 if ki0 == kTSize then (ki2, irrelevantDec)
                  else if erased dec then (ki2, dec) -- do not count erased args in
                 else (unionKind ki0 ki2, dec)
                else (ki2, if argKind ki0 `irrelevantFor` (predKind ki2)
                            then irrelevantDec
                            else dec)
          -- the kind of the Pi-type is the kind of its target (codomain)
          return (VSort s, Kinded ki' $ Quant pisig (TBind n (Domain t1e ki1 dec')) t2e)

      Quant Pi (TMeasure (Measure mu)) t2 -> do
        mue <- mapM checkSize mu
        (s, Kinded ki2 t2e) <- inferType t2
        return (VSort s, Kinded ki2 $ Quant Pi (TMeasure (Measure mue)) t2e)

      Quant Pi (TBound (Bound ltle (Measure mu) (Measure mu'))) t2 -> do
        (mue,mue') <- checkingDom $ do
          mue  <- checkingDom $ mapM checkSize mu
          mue' <- mapM checkSize mu'
          return (mue,mue')
        (s, Kinded ki2 t2e) <- inferType t2
        return (VSort s, Kinded ki2 $ Quant Pi (TBound (Bound ltle (Measure mue) (Measure mue'))) t2e)

      Sing e1 t -> do
        (s, Kinded ki te) <- inferType t
        tv <- whnf' te
        Kinded ki1 e1e <- checkExpr e1 tv
        return (VSort $ s, Kinded (intersectKind ki $ succKind ki1) -- not sure how useful the intersection is, maybe just ki is good enough
                             $ Sing e1e te)

{- Not safe to infer pairs because of irrelevance!
      Pair e1 e2 -> do
        (tv1, Kinded k1 e1) <- inferExpr e1
        (tv2, Kinded k2 e2) <- inferExpr e2
        let ki = unionKind k1 k2
            tv = prod tv1 tv2
        return (tv, Kinded ki $ Pair e1 e2)
-}

      App (Proj Pre p) e  -> inferProj e Pre p
      App e (Proj Post p) -> inferProj e Post p

      App e1 e2 -> checkingCon False $ do
        (v, Kinded ki1 e1e) <- inferExpr e1
        (Kinded ki2 (_, e2e), bv) <- checkApp e2 v
        -- the kind of the application is the kind of its head
        return (bv, Kinded ki1 $ App e1e e2e)
{-
            v <- force v -- if v is a corecursively defined type in Set, unfold!
            case v of
               VQuant Pi x (Domain av _ dec) env b -> do
                  (v2,e2e) <-
                    if inferable e2 then do
                  -- if e2 has a singleton type, we should not take v2 = whnf e2
                  -- but use the single value of e2
                  -- this is against the spirit of bidir. checking
                           -- if checking a type we need to resurrect
                           (av', Kinded _ e2e) <- applyDec dec $ inferExpr e2
                           case av' of
                              VSing v2 av'' -> do subtype av' av
                                                  return (v2,e2e)
                              _ -> do checkSubtype e2e av' av
                                      v2 <- whnf' e2e
                                      return (v2, e2e)
                         else do
                           Kinded _ e2e <- applyDec dec $ checkExpr e2 av
                           v2 <- whnf' e2
                           return (v2, e2e)
                  bv <- whnf (update env x v2) b
                  -- the kind of the application is the kind of its head
                  return (bv, Kinded ki1 $ App e1e (if erased dec then erasedExpr e2e else e2e))
-- if e1e==Irr then Irr else if e2e==Irr then e1e else App e1e [e2e])
               _ -> throwErrorMsg $ "inferExpr : expected Pi with expression : " ++ show e1 ++ "," ++ show v
-}

--      App e1 (e2:el) -> inferExpr $ (e1 `App` [e2]) `App` el
      -- 2012-01-22 no longer infer constructors
      (Def id@(DefId {idKind, idName = name})) | not (conKind idKind) -> do -- traceCheckM ("infer defined head " ++ show n)
         mitem <- errorToMaybe $ lookupName1 $ unqual name
         case mitem of -- first check if it is also a var name
           Just item -> do -- we are inside a mutual declaration (not erased!)
             let pol  = (polarity $ decor $ domain item)
             let upol = (polarity $ upperDec item)
             mId <- asks checkingMutualName
             case mId of
               Just srcId ->
                 -- we are checking constructors or function bodies
                 addPosEdge srcId id upol
               Nothing ->
                 -- we are checking signatures
                 enter ("recursive occurrence of " ++ show name ++ " not strictly positive") $
                   leqPolM pol upol
             return (typ $ domain item, Kinded (kind $ domain item) $ e)
           Nothing -> -- otherwise, it is not the data type name just being defined
                 do sige <- lookupSymbQ name
                    case sige of
                      -- data types have always kind Set 0!
                      (DataSig { symbTyp = tv }) -> return (tv, Kinded (symbolKind sige) e)
                      (FunSig  { symbTyp = tv }) -> return (tv, Kinded (symbolKind sige) e)
                      -- constructors are always terms
                      (ConSig  { symbTyp = tv }) -> returnSing (Kinded kTerm e) tv  -- constructors have sing.type!
                      (LetSig  { symbTyp = tv }) -> return (tv, Kinded (symbolKind sige) e) -- return $ vSing v tv
{-
      (Con _ n) -> do sig <- gets signature
                      case (lookupSig n sig) of
      (Let n) -> do sig <- gets signature
                    case (lookupSig n sig) of
-}
      _ -> throwErrorMsg $ "cannot infer type of " ++ show e
     ) >>= \ tv -> ask >>= \ ce ->
         traceCheck ("inferExpr: " ++ show (renaming ce) ++ ";" ++ show (context ce) ++ " |- " ++ show e ++ " :=> " ++ show tv ++ " in env" ++ show (environ ce)) $
--         traceCheck ("inferExpr: " ++ show e ++ " :=> " ++ show tv) $
           return tv


{- BAD IDEA!
improveDec :: Dec -> TVal -> Dec
improveDec dec v = if v == VSet || v == VSize then erased else dec
-}

{-
-- entry point 3: resurrects
checkType :: Expr -> TypeCheck Extr
checkType e = (resurrect $ checkType' e) `throwTrace` ("not a type: " ++ show e )

checkType' :: Expr -> TypeCheck Extr
checkType' e = case e of
    Sort s -> return e
    Pi dec x t1 t2 -> do
        t1e <- checkType' t1
        -- ignore erasure flag in types!
--        t1v <- whnf' t1e
--        new' x (Domain (Dec False) t1v) $ do
        addBind x (Dec False) t1e $ do
          t2e <- checkType' t2
          return $ Pi dec x t1e t2e  -- Pi (improveDec dec t1v) x t1e t2e
    _ -> checkExpr' e $ VSort Set
-}

checkType :: Expr -> TypeCheck (Kinded Extr)
checkType t =
  enter ("not a type: " ++ show t) $
    resurrect $ do
      (s, te) <- inferType t
      leqSort Pos s (Set VInfty)
      return te

checkSmallType :: Expr -> TypeCheck (Kinded Extr)
checkSmallType t =
  enter ("not a set: " ++ show t) $
    resurrect $ do
      (s, te) <- inferType t
      case s of
        Set VZero -> return te
        CoSet{} -> return te
        _ -> throwErrorMsg $ "expected " ++ show s ++ " to be Set or CoSet _"

{-
-- small type
checkSmallType :: Expr -> TypeCheck Extr
checkSmallType e  = (resurrect $ checkExpr' e $ VSort Set) `throwTrace` ("not a set: " ++ show e )
-}

-- check telescope and add bindings to contexts
checkTele :: Telescope -> TypeCheck a -> TypeCheck (ETelescope, a)
checkTele (Telescope tel) k = loop tel where
  loop tel = case tel of
    []                                  -> (emptyTel,) <$> k
    tb@(TBind x (Domain t _ dec)) : tel -> do
      Kinded ki te <- checkType t
      let tb = TBind x (Domain te (predKind ki) dec)
      (tel, a) <- addBind tb $ loop tel
      return (Telescope $ tb : telescope tel, a)

-- the integer argument is the number of the clause, used just for user feedback
checkCases :: Val -> TVal -> [Clause] -> TypeCheck (Kinded [EClause])
checkCases = checkCases' 1

checkCases' :: Int -> Val -> TVal -> [Clause] -> TypeCheck (Kinded [EClause])
checkCases' i v tv [] = return $ Kinded NoKind []
checkCases' i v tv (c : cl) = do
    Kinded k1 ce  <- checkCase i v tv c
    Kinded k2 cle <- checkCases' (i + 1) v tv cl
    return $ Kinded (unionKind k1 k2) $ ce : cle

checkCase :: Int -> Val -> TVal -> Clause -> TypeCheck (Kinded EClause)
checkCase i v tv cl@(Clause _ [p] mrhs) = enter ("case " ++ show i) $
  -- traceCheck ("checking case " ++ show i) $
    do
      -- clearDots -- NOT NEEDED
      (flex,ins,cxt,vt,pe,pv,absp) <- checkPattern neutral [] emptySub tv p
      local (\ _ -> cxt) $ do
        mapM_ (checkGoal ins) flex
        tel <- getContextTele -- TODO!
        case (absp,mrhs) of
           (True,Nothing) -> return $ Kinded NoKind (Clause tel [pe] Nothing)
           (False,Nothing) -> throwErrorMsg ("missing right hand side in case " ++ showCase cl)
           (True,Just rhs) -> throwErrorMsg ("absurd pattern requires no right hand side in case " ++ showCase cl)
           (False,Just rhs) -> do
              -- pv <- whnf' (patternToExpr p) -- DIFFICULT FOR DOT PATTERNS!
      --        vp <- patternToVal p -- BUG: INTRODUCES FRESH GENS, BUT THEY HAVE ALREADY BEEN INTRODUCED IN checkPattern
              addRewrite (Rewrite v pv) [vt] $ \ [vt'] -> do
                Kinded ki rhse <- checkRHS ins rhs vt'
                return $ Kinded ki (Clause tel [pe] (Just rhse))
                -- [rhs'] <- solveAndModify [rhs] (environ cxt)
                -- return (Clause [p] rhs')

-- type check a function

checkFun :: Type -> [Clause] -> TypeCheck (Kinded [EClause])
checkFun t cl = do
  tv <- whnf' t
  checkClauses tv cl

-- the integer argument is the number of the clause, used just for user feedback
checkClauses :: TVal -> [Clause] -> TypeCheck (Kinded [EClause])
checkClauses = checkClauses' 1

checkClauses' :: Int -> TVal -> [Clause] -> TypeCheck (Kinded [EClause])
checkClauses' i tv [] = return $ Kinded NoKind ([])
checkClauses' i tv (c:cl) = do
    Kinded ki1 ce  <- checkClause i tv c
    Kinded ki2 cle <- checkClauses' (i + 1) tv cl
    return $ Kinded (unionKind ki1 ki2) $ (ce : cle)

-- checkClause i tv cl = (cl', cle)
-- checking one equation cl of a function at type tv
-- solve size constraints
-- substitute solution into clause, resulting in cl'
-- return also extracted clause cle
checkClause :: Int -> TVal -> Clause -> TypeCheck (Kinded EClause)
checkClause i tv cl@(Clause _ pl mrhs) = enter ("clause " ++ show i) $ do
  -- traceCheck ("checking function clause " ++ show i) $
    -- clearDots -- NOT NEEDED
    (flex,ins,cxt,tv0,ple,plv,absp) <- checkPatterns neutral [] emptySub tv pl
    -- 2013-03-30 When checking the rhs, we only allow new size hypotheses
    -- if they do not break any valuation of the existing hypotheses.
    -- See ICFP 2013 paper.
    -- We exclude cofuns here, for experimentation.
    -- Note that cofuns need not be SN, so the strict consistency may be
    -- not necessary.
    local (\ _ -> cxt { consistencyCheck = (mutualCo cxt == Ind) }) $ do
      mapM_ (checkGoal ins) flex
{-
      dots <- openDots
      unless (null dots) $
        recoverFailDoc $ text "the following dotted constructors could not be confirmed: " <+> prettyTCM dots
-}
      -- TODO: insert meta var solution in dot patterns
      tel <- getContextTele -- WRONG TELE, has VGens for DotPs
      case (absp,mrhs) of
         (True,Nothing) -> return $ Kinded NoKind (Clause tel ple Nothing)
         (False,Nothing) -> throwErrorMsg ("missing right hand side in clause " ++ show cl)
         (True,Just rhs) -> throwErrorMsg ("absurd pattern requires no right hand side in clause " ++ show cl)
         (False,Just rhs) -> do
            Kinded ki rhse <- checkRHS ins rhs tv0
            env  <- getEnv
            [rhse] <- solveAndModify [rhse] env
            return $ Kinded ki (Clause tel ple (Just rhse))


-- * Pattern checking ------------------------------------------------

type Substitution = Valuation -- [(Int,Val)]

emptySub :: Substitution
emptySub = emptyVal

sgSub :: Int -> Val -> Substitution
sgSub = sgVal

lookupSub :: Int -> Substitution -> Maybe Val
lookupSub i = lookup i . valuation

type DotFlex = (Int,(Expr,Domain))

-- left over goals
data Goal
    = DotFlex Int (Maybe Expr) Domain
      -- ^ @Just@ : Flexible variable from inaccessible pattern.
      -- ^ @Nothing@ : Flexible variable from hidden function type.
    | MaxMatches Int TVal
    | DottedCons Dotted Pattern TVal
  deriving Show

-- checkPatterns is initially called with an empty local context
-- in the type checking monad
checkPatterns :: Dec -> [Goal] -> Substitution -> TVal -> [Pattern] -> TypeCheck ([Goal],Substitution,TCContext,TVal,[EPattern],[Val],Bool)
checkPatterns dec0 flex ins v pl =
  case v of
    VMeasured mu vb -> setMeasure mu $ checkPatterns dec0 flex ins vb pl
    VGuard beta vb -> addBoundHyp beta $ checkPatterns dec0 flex ins vb pl
{-
    VGuard beta vb -> throwErrorMsg $ "checkPattern at type " ++ show v ++ " --- introduction of constraints not supported"
-}
    _ -> case pl of
      [] -> do cxt <- ask
               return (flex,ins,cxt,v,[],[],False)
      (p:pl') -> do (flex',ins',cxt',v',pe,pv,absp) <- checkPattern dec0 flex ins v p
                    local (\ _ -> cxt') $ do
                      (flex'',ins'',cxt'',v'',ple,plv,absps) <- checkPatterns dec0 flex' ins' v' pl'
                      return (flex'',ins'',cxt'',v'', pe:ple, pv:plv, absp || absps) -- if pe==IrrP then ple else pe:ple)

{-
checkPattern dec0 flex subst tv p = (flex', subst', cxt', tv', pe, pv, absp)

Input :
  dec0  : context in which pattern occurs (irrelevant, parametric, recursive)
          are we checking an erased argument? (constr. pat. needs to be forced!)
  flex  : list of pairs (flexible variable, its dot pattern + supposed type)
  subst : list of pairs (flexible variable, its valuation)
  cxt   : in monad, containing
    rho   : binding of variables to values
    delta : binding of generic values to their types
  tv    : type of the expression \ p -> t
  p     : the pattern to check

Output
  tv'   : type of t
  pe    : erased pattern
  pv    : value of pattern (this is in essence whnf' pe,
            but we cannot evaluate because of dot patterns)
  absp  : did we encounter an absurd pattern
-}

checkPattern :: Dec -> [Goal] -> Substitution -> TVal -> Pattern -> TypeCheck ([Goal],Substitution,TCContext,TVal,EPattern,Val,Bool)
checkPattern dec0 flex ins tv p = -- ask >>= \ TCContext { context = delta, environ = rho } -> trace ("checkPattern" ++ ("\n  dot pats: " +?+ show flex) ++ ("\n  substion: " +?+ show ins) ++ ("\n  environ : " +?+ show rho) ++ ("\n  context : " +?+ show delta) ++ "\n  pattern : " ++ show p ++ "\n  at type : " ++ show tv ++ "\t<>") $
 enter ("pattern " ++ show p) $ do
  tv <- force tv
  case tv of
    -- record type can be eliminated
    VApp (VDef (DefId DatK d)) vl ->
      case p of
        ProjP proj -> do
          tv <- projectType tv proj VIrr -- do not have record value here
          cxt <- ask
          return (flex, ins, cxt, tv, p, VProj Post proj, False)
{-
          mfs <- getFieldsAtType d vl
          case mfs of
            Nothing -> failDoc (text "cannot eliminate type" <+> prettyTCM tv <+> text "with projection pattern" <+> prettyTCM p)
            Just ptvs ->
              case lookup proj ptvs of
                Nothing -> failDoc (text "record type" <+> prettyTCM tv <+> text "does not know projection" <+> text proj)
                Just tv -> do
                  tv <- piApp tv VIrr -- cut of record arg
                  cxt <- ask
                  return (flex, ins, cxt, tv, p, VProj proj, False)
-}
        _ -> failDoc (text "cannot eliminate type" <+> prettyTCM tv <+> text "with a non-projection pattern")

    -- intersection type
    VQuant Pi x dom@(Domain av ki Hidden) fv -> do
      -- introduce new flexible variable
      newWithGen x dom $ \ i xv -> do
        tv <- fv `app` xv
        checkPattern dec0 (DotFlex i Nothing dom : flex) ins tv p

    -- function type can be eliminated
    VQuant Pi x (Domain av ki dec) fv -> do
{-
       let erased' = er || erased dec
       let decEr   = if erased' then irrelevantDec else dec -- dec {erased = erased'}
-}
       let decEr = dec `compose` dec0
       let domEr   =  (Domain av ki decEr)
       case p of

         -- treat successor pattern here, because of admissibility check
         SuccP p2 -> do
                 when (av /= vSize) $ throwErrorMsg "checkPattern: expected type Size"
                 when (isSuccessorPattern p2) $ cannotMatchDeep p tv

                 co <- asks mutualCo
                 when (co /= CoInd) $
                   throwErrorMsg ("successor pattern only allowed in cofun")

                 enterDoc (text ("checkPattern " ++ show p ++" : matching on size, checking that target") <+> prettyTCM tv <+> text "ends in correct coinductive sized type") $
                   underAbs x domEr fv $ \ i _ bv -> endsInSizedCo i bv

                 cxt <- ask
                 -- 2012-02-05 assume size variable in SuccP to be < #
                 let sucTy = (vFinSize `arrow` vFinSize)
                 (flex',ins',cxt',tv',p2e,p2v,absp) <- checkPattern decEr flex ins sucTy p2
                 -- leqVal Mixed delta' VSet VSize av -- av = VSize
                 let pe = SuccP p2e
                 let pv = VSucc p2v
--                 pv0 <- local (\ _ -> cxt') $ whnf' $ patternToExpr pe
                 -- pv0 <- patternToVal p -- RETIRE patternToVal
                 -- pv  <- up False pv0 av -- STUPID what can be eta-exanded at type Size??
                 vb  <- app fv pv
{-
                 endsInCoind <- endsInSizedCo pv vb
                 when (not endsInCoind) $ throwErrorMsg $ "checkPattern " ++ show p ++" : cannot match on size since target " ++ show tv ++ " does not end in correct coinductive sized type"
-}
                 return (flex',ins',cxt',vb,pe,pv,absp)

         -- other patterns: no need to know about result type
         _ -> do
           (flex',ins',cxt',pe,pv,absp) <- checkPattern' flex ins domEr p
           -- traceM ("checkPattern' returns " ++ show (flex',ins',cxt',pe,pv,absp))
           vb  <- app fv pv
           vb  <- substitute ins' vb  -- from ConP case -- ?? why not first subst and then whnf?
           -- traceCheckM ("Returning type " ++ show vb)
           return (flex',ins',cxt',vb,pe,pv,absp)

    _ -> throwErrorMsg $ "checkPattern: expected function type, found " ++ show tv

-- TODO: refactor with monad transformers
-- put absp into writer monad

turnIntoVarPatAtUnitType :: TVal -> Pattern -> TypeCheck Pattern
turnIntoVarPatAtUnitType (VApp (VDef (DefId DatK n)) _) p@(ConP pi c []) =
  flip (ifM $ isUnitData n) (return p) $ do
    let x = fresh "un!t"
    return $ VarP x
turnIntoVarPatAtUnitType _ p = return p

checkPattern' :: [Goal] -> Substitution -> Domain -> Pattern -> TypeCheck ([Goal],Substitution,TCContext,EPattern,Val,Bool)
checkPattern' flex ins domEr@(Domain av ki decEr) p = do
       p <- turnIntoVarPatAtUnitType av p
       case p of
          SuccP{} -> failDoc (text "successor pattern" <+> prettyTCM p <+> text "not allowed here")

          PairP p1 p2 -> do
            av <- force av
            case av of
             VQuant Sigma y dom1@(Domain av1 ki1 dec1) fv -> do
              (flex, ins, cxt, pe1, pv1, absp1) <-
                 checkPattern' flex ins (Domain av1 ki1 $ dec1 `compose` decEr) p1
              av2 <- app fv pv1
              (flex, ins, cxt, pe2, pv2, absp2) <-
                 local (const cxt) $
                   checkPattern' flex ins (Domain av2 ki decEr) p2
              return (flex, ins, cxt, PairP pe1 pe2, VPair pv1 pv2, absp1 || absp2)
             _ -> failDoc (text "pair pattern" <+> prettyTCM p <+> text "could not be checked against type" <+> prettyTCM av)
{-
   (x : Sigma y:A. B) -> C
     =iso= (y : A) -> (x' : B) -> C[(y,x')/x]

   (x : Sigma y:V. <B;rho1>) -> <C;rho2>
     =iso= (y : V) -> <(x': B) -> C; ?? x=(y,x')>
 -}
{-
            case av of
              VQuant Sigma y dom1@(Domain av1 ki1 dec1) env1 a2 -> do
                let x' = x ++ "#2"
                    ep = Pair (Var y) (Var x')
                    tv = VQuant Pi y dom1 env1 $
                           Quant x' (Domain a2
-}

          ProjP proj -> failDoc (text "cannot eliminate type" <+> prettyTCM av <+> text "with projection pattern" <+> prettyTCM p)

          VarP y -> do
            new y domEr $ \ xv -> do
              cxt' <- ask
              p' <- case av of
                       VBelow Lt v -> flip SizeP y <$> toExpr v
                       _ -> return p
              return (flex, ins, cxt', maybeErase $ p', xv, False)

{- checking bounded size patterns

    ex : [i : Size] -> [j : Below< i] -> ...
    ex i (j < i) = ...

  type of pattern : Below< i needs to cover type of parameter Below< i

    zero : [j : Size] -> Nat $j   -- need to hold a "sized con type"
    zero : [j < i]    -> Nat i

    ex : [i : Size] -> (n : Nat i) -> ...
    ex i (zero (j < i) = ...

  type of size-pat : Below< i

-}
          SizeP e y -> do -- pattern (z > y), y is the bound variable, z the bound of z
            e <- resurrect $ checkSize e -- (Var z)
            newWithGen y domEr $ \ j xv -> do
{-
               VGen k <- whnf' (Var z)
               addSizeRel j 1 k $ do  -- j < k
-}
               ve <- whnf' e
               addBoundHyp (Bound Lt (Measure [xv]) (Measure [ve])) $ do
                 subtype av (VBelow Lt ve)
                 cxt' <- ask
                 return (flex, ins, cxt', maybeErase $ SizeP e y, xv, False)

          AbsurdP -> do
                 when (isFunType av) $ throwErrorMsg ("absurd pattern " ++ show p ++ " does not match function types, like " ++ show av)
                 cxt' <- ask
                 return (MaxMatches 0 av : flex, ins, cxt', maybeErase $ AbsurdP, VIrr, True)
{-
                 cenvs <- matchingConstructors av  -- TODO: av might be MVar
                                                   -- need to be postponed
                 case cenvs of
                    [] -> do bv   <- whnf (update env x VIrr) b
                             cxt' <- ask
                             return (flex, ins, cxt', bv, maybeErase $ AbsurdP, True)
                    _ -> throwErrorMsg $ "type " ++ show av ++ " of absurd pattern not empty"
-}

          -- always expand defined patterns!
          p@(ConP pi n ps) | coPat pi == DefPat -> do
            checkPattern' flex ins domEr =<< expandDefPat p

--          ConP pi n pl | not $ dottedPat pi -> do
          ConP pi n pl -> do

                 -- disambiguate constructor first
                 n <- disambigCon n av

                 let co     = coPat pi
                     dotted = dottedPat pi

                 -- First check that we do not match against an irrelevant argument.
                 unless dotted $ nonDottedConstructorChecks n co pl
{- TODO
                 enter ("can only match non parametric arguments") $
                   leqPolM (polarity dec) (pprod defaultPol)
-}
                 (vc,(flex',ins',cxt',vc',ple,pvs,absp)) <- checkConstructorPattern co n pl

                 when (isFunType vc') $ throwErrorMsg ("higher-order matching of pattern " ++ show p ++ " of type " ++ show vc' ++ " not allowed")
                 let flexgen = concat $ map (\ g -> case g of
                        DotFlex i _ _ -> [i]
                        _ -> []) flex'
                     -- fst $ unzip flex'
--                  av1 <- sing (environ cxt') (patternToExpr p) vc'
--                  av2 <- sing (environ cxt') (patternToExpr p) av
--                  subst <- local (\ _ -> cxt') $ inst flexgen VSet av1 av2


                 -- need to evaluate the erased pattern!
                 let pe = ConP pi n ple -- erased pattern
                 -- dot <- if dottedPat pi then newDotted p else return notDotted
                 dot <- if dottedPat pi then mkDotted True else return notDotted
                 pv0 <- mkConVal dot co n pvs vc
                 -- OLD: let pv0 = VDef (DefId (ConK co) n) `VApp` pvs
{-
                 let epe = patternToExpr pe
                 pv0 <- local (\ _ -> cxt') $ whnf' epe
--                 pv0 <- patternToVal p -- THIS USE should be ok, since the new GENs are not in the global context yet, only in cxt' -- NO LONGER ok with erasure!
                 -- traceM $ "sucessfully computed value " ++ show pv0 ++ " of pattern " ++ show epe
-}

                 subst <- local (\ _ -> cxt') $ do
                   case av of  -- TODO: need subtyping-unify instead of unify??
                     VSing vav av0 -> do
                       vav <- whnfClos vav
                       inst Pos flexgen av0 pv0 vav
                     _ -> unifyIndices flexgen vc' av  -- vc' <= av ?!
                   -- THIS IMPLEMENTATION RELIES HEAVILY ON INJECTIVITY OF DATAS

{- moved to checkRHS
                 -- apply substitution to measures in environment
                 let mmu = (envBound . environ) cxt'
                 mmu' <- Traversable.mapM (substitute subst) mmu
-}
{-
                 ins'' <- compSubst ins' subst
                 vb <- substitute ins'' vb
                 delta' <- substitute ins'' delta'
-}
                 ins''   <- compSubst ins' subst -- 2010-07-27 not ok to switch!
                 delta'' <- substitute ins'' (context cxt')
                 traceCheckM $ "delta'' = " ++ show delta''
                 av  <- substitute ins'' av  -- 2010-09-22: update av
                 pv  <- up False pv0 av

                 -- if the constructor was dotted, make sure it is the only match
                 let flex'' = fwhen dotted (DottedCons dot p av :) flex'
                 return (flex'', ins'', cxt' { context = delta'' },
                         maybeErase pe, pv, absp)
{- DO NOT UPDATE measure here, its done in checkRHS
                 return (flex', ins'', cxt' { context = delta'', environ = (environ cxt') { envBound = mmu' } }, vb',
                         maybeErase pe, absp)
-}


{- UNUSED
          -- If we encounter a dotted constructor, we simply
          -- compute the pattern variable context
          -- and then treat the pattern as dot pattern.
          p@(ConP pi n ps) | dottedPat pi -> do
            (vc,(flex',ins',cxt',vc',ple,pvs,absp)) <-
              checkConstructorPattern (coPat pi) n ps
            local (const cxt') $
              checkPattern' flex ins domEr $ DotP $ patternToExpr p
-}

          DotP e -> do
            -- create an informative, but irrelevant identifier for dot pattern
            let xp = fresh $ "." ++ case e of Var z -> suggestion z; _ -> Util.parens $ show e
            newWithGen xp domEr $ \ k xv -> do
                       cxt' <- ask
                       -- traceCheck ("Returning type " ++ show vb) $
                       return (DotFlex k (Just e) domEr : flex
                              ,ins
                              ,cxt'
                              ,maybeErase $ DotP e -- $ Var xp -- DotP $ Meta k -- e -- Meta k
                              -- ,maybeErase $ -- AbsurdP -- VarP $ show e
                              ,xv
                              ,False) -- TODO: Erase in e/ Meta subst!
{- original code
                    do let (k, delta') = cxtPush dec av delta
                       vb <- whnf (update env x (VGen k)) b
                       return ((k,(e,Domain av dec)):flex
                              ,ins
                              ,rho
                              ,delta'
                              ,vb)
-}

    where
      maybeErase p = if erased decEr then ErasedP p else p

      checkConstructorPattern co n pl = do
                 when (isFunType av) $ throwErrorMsg ("higher-order matching of pattern " ++ show p ++ " at type " ++ show av ++ " not allowed")
-- TODO: ensure that matchings against erased arguments are forced
--                 when (erased dec) $ throwErrorMsg $ "checkPattern: cannot match on erased argument " ++ show p ++ " : " ++ show av

                 ConSig {conPars, lhsTyp = sz, recOccs, symbTyp = vc, dataName, dataPars} <- lookupSymbQ n

                 -- the following is a hack to still support old-style
                 --   add .($ i) (zero i) ...
                 -- fun defs:  if (zero i) is matched against (Nat flexvar$i)
                 -- we use the old constructor type [i : Size] -> Nat $i
                 -- else, the new one [j < i] -> Nat i
                 let flexK k (DotFlex k' _ _) = k == k'
                     flexK k _ = False
                     -- use lhs con type only if sizeindex is not a rigid var
                     isFlex (VGen k) = List.any (flexK k) flex
                     isFlex _ = True
                     isSz = if co == Cons then sz else Nothing
                 vc <- instConLType n conPars vc isSz isFlex dataPars =<< force av
{-
                 vc <- case sz of
                         Nothing -> instConType n nPars vc =<< force av
                         Just vc -> instConType n (nPars+1) vc =<< force av
-}

                 -- (flex',ins',cxt',vc',ple,pvs,absp) <-
                 (vc,) <$> checkPatterns decEr flex ins vc pl


      -- These checks are only relevant if a constructor is an actual match.
      nonDottedConstructorChecks n co pl = do
        ConSig {conPars, lhsTyp = sz, recOccs, symbTyp = vc, dataName, dataPars} <- lookupSymbQ n

        -- check that size argument of coconstr is dotted
        when (co == CoCons && isJust sz) $ do
          let sizep = head pl  -- 2012-01-22: WAS (pl !! nPars)
          unless (isDotPattern sizep) $
            throwErrorMsg $ "in pattern " ++ show p  ++ ", coinductive size sub pattern " ++ show sizep ++ " must be dotted"

        when (not $ decEr `elem` map Dec [Const,Rec]) $
          recoverFail $ "cannot match pattern " ++ show p ++ " against non-computational argument"
        -- check not to match non-trivially against erased stuff
        when (decEr == Dec Const) $ do
          let failNotForced = recoverFail $ "checkPattern: constructor " ++ show n ++ " of non-computational argument " ++ show p ++ " : " ++ show av ++ " not forced"
          mcenvs <- matchingConstructors av
          case mcenvs of
             Nothing -> do -- now check whether dataName is a record type
               DataSig { constructors } <- lookupSymb dataName
               unless (length constructors == 1) $ failNotForced
               return ()
             Just [] -> recoverFail $ "checkPattern: no constructor matches type " ++ show av
             Just [(ci, _)] | cName ci == n -> return ()
             _ -> failNotForced




{- New treatment of size matching  (see examples/Sized/Cody.ma)

sized data O : Size -> Set
{ Z : [i : Size] -> O ($ i)
; S : [i : Size] -> O i -> O ($ i)
; L : [i : Size] -> (Nat -> O i) -> O ($ i)
; M : [i : Size] -> O i -> O i -> O ($ i)
}

fun deep : [i : Size] -> O i -> Nat -> Nat
{ deep i4 (M i3 (L j2 f) (S i2 (S i1 (S i x)))) n
  = deep _ (M _ (L _ (pre _ f)) (S _ (f n))) (succ (succ (succ n)))
; deep i x n = n
}

Explicit form:  Size variables and their constraints are noted explicitely,
to be able to do untyped call extraction in the termination module.

 deep i4
  (M (i4 > i3)
       (L (i3 > j2) f)
       (S (i3 > i2)
            (S (i2 > i1)
                 (S (i1 > i) x)))) n
  = deep _ (M _ (L _ (pre _ f)) (S _ (f n))) (succ (succ (succ n)))

i4, i3, ... are all rigid variables with constraints between them.
There is a tree hierarchy, but I do not know whether I can exploit
this.

  i4 > i3 > i2 > i1 > i
          > j3

This could be stored in a union-find-like data structure, or just in
the constraint matrix.

How to pattern match?

  id : [i : Size] -> List i -> List i
  id i (cons (i > j) x xs) = cons j x (id j xs)

Only a size variable matches a size arguments

  match  (cons (i > j) x xs)   against   List i
  get    cons : [j : Size] -> Nat -> List j -> List ($ j)
  yield  x : Nat, xs : List j, cons j x xs : List ($ j)
  check  List ($ j) <= List i
 -}

-- checkDot does not need to extract
-- 2012-01-25 now we do since "extraction" turns also con.terms into records
checkGoal :: Substitution -> Goal -> TypeCheck ()
checkGoal subst (DotFlex i me it) = enter ("dot pattern " ++ show me) $
  case lookupSub i subst of
    Nothing -> recoverFail $ "not instantiated"
    Just v -> whenJust me $ \ e -> do
      tv <- substitute subst (typ it)
      ask >>= \ ce -> traceCheckM ("checking dot pattern " ++ show ce ++ " |- " ++ show e ++ " : " ++ show (decor it) ++ " " ++ show tv)
--      applyDec (decor it) $ do
      resurrect $ do -- consider a DotP e always as irrelevant!
        e <- valueOf <$> checkExpr e tv
        v' <-  whnf' e -- TODO: has subst erased terms?
        enterDoc (text "inferred value" <+> prettyTCM v <+> text "does not match given dot pattern value" <+> prettyTCM v') $
          leqVal Pos tv v v' -- WAS: eqVal
checkGoal subst (MaxMatches n av) = do
  traceCheckM ("checkGoal _ $ MaxMatches " ++ show n ++ " $ " ++ show av)
  av' <- substitute subst av
  traceCheckM ("checkGoal _ $ MaxMatches " ++ show n ++ " $ " ++ show av')
  -- av' <- reval av'
  -- traceCheckM ("checkGoal: reevalutated " ++ show av')
  mcenvs <- matchingConstructors av'
  traceCheckM ("checkGoal matching constructors = " ++ show mcenvs)
  maybe (recoverFail $ "not a data type: " ++ show av')
   (\ cenvs ->
      if length cenvs > n then recoverFail $
        if n==0 then "absurd pattern does not match since type " ++ show av' ++ " is not empty"
         else
           "more than one constructor matches type " ++ show av'
       else return ())
   mcenvs
checkGoal subst (DottedCons dot p av)
  | isDotted dot =
      enterDoc (text "confirming dotted constructor" <+> prettyTCM p) $ do
        checkGoal subst (MaxMatches 1 av)
  | otherwise    = return ()

checkRHS :: Substitution -> Expr -> TVal -> TypeCheck (Kinded Extr)
checkRHS ins rhs v = do
   traceCheckM ("checking rhs " ++ show rhs ++ " : " ++ show v)
   enter "right hand side" $ do
     -- first update measure according to substitution for dot variables
     cxt <- ask
     let rho = environ cxt
     mmu' <- Traversable.mapM (substitute ins) (envBound rho)
     local (\ _ -> cxt { environ = rho { envBound = mmu' }}) $
       activateFuns $
         checkExpr rhs v



-- TODO type directed unification

-- unifyIndices flex tv1 tv2
-- tv1 = D pars  inds  is the type of the pattern
-- tv2 = D pars' inds' is the type matched against
-- Note that in this case we can unify without using the principle of
-- injective data type constructors,
-- we are only calling unifyIndices from the constructor pattern case
-- in Checkpattern
unifyIndices :: [Int] -> Val -> Val -> TypeCheck Substitution
unifyIndices flex v1 v2 = ask >>= \ cxt -> enterDoc (text ("unifyIndices " ++ show (context cxt) ++ " |-") <+> prettyTCM v1 <+> text ("?<=" ++ show Pos) <+> prettyTCM v2) $ do
-- {-
  case (v1,v2) of
    (VSing _ v1, VApp (VDef (DefId DatK d2)) vl2) ->
      flip (unifyIndices flex) v2 =<< whnfClos v1
    (VApp (VDef (DefId DatK d1)) vl1, VApp (VDef (DefId DatK d2)) vl2) | d1 == d2 -> do
      (DataSig { numPars = np, symbTyp = tv, positivity = posl}) <- lookupSymbQ d1
      instList posl flex tv vl1 vl2 -- unify also parameters to solve dot patterns
    _ ->
-- -}
         inst Pos flex vTopSort v1 v2
-- throwErrorMsg ("unifyIndices " ++ show v1 ++ " =?= " ++ show v2 ++ " failed, not applied to data types")

-- unify, but first produce whnf
instWh :: Pol -> [Int] -> TVal -> Val -> Val -> TypeCheck Substitution
instWh pos flex tv w1 w2 = do
  v1 <- whnfClos w1
  v2 <- whnfClos w2
  inst pos flex tv v1 v2

-- | Check occurrence and return singleton substitution.
assignFlex :: Int -> Val -> TypeCheck Substitution
assignFlex k v = do
  unlessM (nocc [k] v) $
    failDoc $
      text "variable " <+> prettyTCM (VGen k) <+>
      text " may not occur in " <+> prettyTCM v
  return $ sgSub k v

-- match v1 against v2 by unification , yielding a substition
inst :: Pol -> [Int] -> TVal -> Val -> Val -> TypeCheck Substitution
inst pos flex tv v1 v2 = ask >>= \ cxt -> enterDoc (text ("inst " ++ show (context cxt) ++ " |-") <+> prettyTCM v1 <+> text ("?<=" ++ show pos) <+> prettyTCM v2 <+> colon <+> prettyTCM tv) $ do
--  case tv of
--    (VPi dec x av env b) ->
  case (v1,v2) of
    (VGen k, VGen j) | k == j -> return emptySub
    (VGen k, _) | elem k flex -> assignFlex k v2
    (_, VGen k) | elem k flex -> assignFlex k v1

    -- injectivity of data type constructors is unsound in general
    (VApp (VDef (DefId DatK d1)) vl1,
     VApp (VDef (DefId DatK d2)) vl2) | d1 == d2 ->  do
         (DataSig { numPars, symbTyp = tv, positivity = posl }) <- lookupSymbQ d1
         instList' numPars posl flex tv vl1 vl2
           -- ignore parameters (first numPars args)
           -- this is sound because we have irrelevance for parameters
           -- we assume injectivity for indices

    -- Constructor applications are represented as VRecord
    (VRecord (NamedRec _ c1 _ dot1) rs1,
     VRecord (NamedRec _ c2 _ dot2) rs2) | c1 == c2 -> do
         alignDotted dot1 dot2
         sige <- lookupSymbQ c1
         instList [] flex (symbTyp sige) (map snd rs1) (map snd rs2)

    (VSucc v1',     VSucc v2')     -> instWh pos flex tv v1' v2'
    (VSucc v,       VInfty)        -> instWh pos flex tv v   VInfty
    (VSing v1' tv1, VSing v2' tv2) -> do
      subst <- inst pos flex tv tv1 tv2
      u1 <- substitute subst v1'
      u2 <- substitute subst v2'
      tv1' <- substitute subst tv1
      inst pos flex tv1' u1 u2 >>= compSubst subst

-- HACK AHEAD
    (VUp v1 _, _) -> inst pos flex tv v1 v2
    (_, VUp v2 _) -> inst pos flex tv v1 v2
--    (VUp v1' a1, VUp v2' a2) -> instList flex [a1,v1'] [a2,v2']
--     (VPi dec x1 av1 env1 b1, VPi dec x2 av2 env2 b2) ->

{- TODO: REPAIR THIS
    _ -> traceCheck ("inst: WARNING! assuming " ++ show (context cxt) ++ " |- " ++ show v1 ++ " == " ++ show v2) $
           return [] -- throwErrorMsg $ "inst: NYI"
 -}
    _ -> do leqVal pos tv v1 v2 `throwTrace` ("inst: leqVal " ++ show v1 ++ " ?<=" ++ show pos ++ " " ++ show v2 ++ " : " ++ show tv ++ " failed")
            return emptySub

instList :: [Pol] -> [Int] -> TVal -> [Val] -> [Val] -> TypeCheck Substitution
instList = instList' 0

-- unify lists, ignoring the first np items
instList' :: Int -> [Pol] -> [Int] -> TVal -> [Val] -> [Val] -> TypeCheck Substitution
instList' np posl flex tv [] [] = return emptySub
instList' np posl flex tv (v1:vl1) (v2:vl2) = do
  v1 <- whnfClos v1
  v2 <- whnfClos v2
  if (np <= 0 || isMeta flex v1 || isMeta flex v2) then
    case tv of
      (VQuant Pi x dom fv) -> do
        let pol = getPol dom  -- WAS: (headPosl posl)
        subst <- inst pol flex (typ dom) v1 v2
        vl1' <- mapM (substitute subst) vl1
        vl2' <- mapM (substitute subst) vl2
        v    <- substitute subst v1
        fv   <- substitute subst fv
        vb   <- app fv v
        subst' <- instList' (np - 1) (tailPosl posl) flex vb vl1' vl2'
        compSubst subst subst'
   else
    case tv of
      (VQuant Pi x dom fv) -> do
        vb   <- app fv v2
        instList' (np - 1) (tailPosl posl) flex vb vl1 vl2
instList' np pos flex tv vl1 vl2 = throwErrorMsg $ "internal error: instList' " ++ show (np,pos,flex,tv,vl1,vl2) ++ " not handled"

headPosl :: [Pol] -> Pol
headPosl [] = mixed
headPosl (pos:_) = pos

tailPosl :: [Pol] -> [Pol]
tailPosl [] = []
tailPosl (_:posl) = posl


isMeta :: [Int] -> Val -> Bool
isMeta flex (VGen k) = k `elem` flex
isMeta _ _ = False

----------------------------------------------------------------------
-- * Substitution into values
----------------------------------------------------------------------

-- | Overloaded substitution of values for generic values (free variables).
class Substitute a where
  substitute :: Substitution -> a -> TypeCheck a

instance Substitute v => Substitute (x,v) where
  substitute subst (x,v) = (x,) <$> substitute subst v

instance Substitute v => Substitute [v] where
  substitute = mapM . substitute

instance Substitute v => Substitute (Maybe v) where
  substitute = Traversable.mapM . substitute

instance Substitute v => Substitute (Map k v) where
  substitute = Traversable.mapM . substitute

instance Substitute v => Substitute (OneOrTwo v) where
  substitute = Traversable.mapM . substitute

instance Substitute v => Substitute (Dom v) where
  substitute = Traversable.mapM . substitute

instance Substitute v => Substitute (Measure v) where
  substitute = Traversable.mapM . substitute

instance Substitute v => Substitute (Bound v) where
  substitute = Traversable.mapM . substitute

instance Substitute v => Substitute (Sort v) where
  substitute = Traversable.mapM . substitute

-- substitute generic variable in value
instance Substitute Val where
  substitute subst v = do -- enterDoc (text "substitute" <$> prettyTCM v) $ do
    let sub v = substitute subst v
    case v of
      VGen k                -> return $ valuateGen k subst
      VApp v1 vl            -> foldM app ==<< (sub v1, sub vl)
      VSing v1 vt           -> vSing ==<< (sub v1, sub vt) -- TODO: Check reevaluation necessary?

      VSucc v1              -> succSize  <$> substitute subst v1
      VMax  vs              -> maxSize   <$> mapM (substitute subst) vs
      VPlus vs              -> plusSizes <$> mapM (substitute subst) vs

      VCase v1 tv1 env cl   -> VCase <$> sub v1 <*> sub tv1 <*> sub env <*> return cl
      VMeasured mu bv       -> VMeasured <$> sub mu <*> sub bv
      VGuard beta bv        -> VGuard <$> sub beta <*> sub bv

      VBelow ltle v         -> VBelow ltle <$> substitute subst v

      VQuant pisig x dom fv -> VQuant pisig x <$> sub dom <*> sub fv
      VRecord ri rs         -> VRecord ri <$> sub rs
      VPair v1 v2           -> VPair <$> sub v1 <*> sub v2
      VProj{}               -> return v

      VLam x env b          -> flip (VLam x) b <$> sub env
      VConst v              -> VConst <$> sub v
      VAbs x i v valu       -> VAbs x i v <$> sub valu
      VClos env e           -> flip VClos e <$> sub env
      VUp v1 vt             -> up False ==<< (sub v1, sub vt)
      VSort s               -> VSort <$> sub s
      VZero                 -> return $ v
      VInfty                -> return $ v
      VIrr                  -> return $ v
      VDef id               -> return $ vDef id  -- because empty list of apps will be rem.
      VMeta x env n         -> flip (VMeta x) n <$> sub env
{- REDUNDANT
      _ -> error $ "substitute: internal error: not defined for " ++ show v
-}

instance Substitute SemCxt where
  substitute subst delta = do
    cxt' <- substitute subst (cxt delta)
    return $ delta { cxt = cxt' }

-- | Substitute in environment.
instance Substitute Env where
  substitute subst (Environ rho mmeas) =
    Environ <$> substitute subst rho <*> substitute subst mmeas

instance Substitute Substitution where
  substitute subst2 subst1 = compSubst subst1 subst2

-- | "merge" substitutions by first applying the second to the first, then
--   appending them @t[sigma][tau] = t[sigma . tau]@
compSubst :: Substitution -> Substitution -> TypeCheck Substitution
compSubst (Valuation subst1) subst2@(Valuation subst2') =
    Valuation . (++ subst2') <$> substitute subst2 subst1

----------------------------------------------------------------------
-- * Size checking
----------------------------------------------------------------------

{- TODO: From a sized data declaration

  sized data D pars : Size -> t
  { c : [j : Size] -> args -> D pars $j ts
  }

  with constructor type

   c : .pars -> [j : Size] -> args -> D pars $j ts

  extract new-style constructor type

   c :  .pars -> [i : Size] -> [j < i : Size] -> args -> D pars i ts

  Then replace in ConSig filed isSized :: Sized  by :: Maybe Expr
  which stores the new-style constructor type

-}

mkConLType :: Int -> Expr -> (Name, Expr)
mkConLType npars t =
  let (Telescope (sizetb : tel), t0) = typeToTele t
  in case spineView t0 of
    (d@(Def (DefId DatK _)), args) ->
      let (pars, sizeindex : inds) = splitAt npars args
          i     = fresh "s!ze"
          args' = pars ++ Var i : inds
          core  = foldl App d args'
          tbi   = TBind i $ sizeDomain irrelevantDec
          tbj   = sizetb { boundDom = belowDomain irrelevantDec Lt (Var i) }
          tel'  = Telescope $ tbi : tbj : tel
      in (i, teleToType tel' core)
    _ -> error $ "conLType " ++ show npars ++ " (" ++ show t ++ "): illformed constructor type"



-- * check wether the data type is sized type


-- check data declaration type
-- called from typeCheckDeclaration (DataDecl{})
-- parameters : number of params, type
szType :: Co -> Int -> TVal -> TypeCheck ()
szType co p tv = doVParams p tv $ \ tv' -> do
    let polsz = if co==Ind then Pos else Neg
    case tv' of
      VQuant Pi x (Domain av ki dec) fv | isVSize av && not (erased dec) && polarity dec == polsz -> return ()
      _ -> throwErrorMsg $ "not a sized type, target " ++ show tv' ++ " must have non-erased domain " ++ show Size ++ " with polarity " ++ show polsz

-- * constructors of sized type

-- check data constructors
-- called from typeCheckConstructor
szConstructor :: Name -> Co -> Int -> TVal -> TypeCheck ()
szConstructor n co p tv = enterDoc (text ("szConstructor " ++ show n ++ " :") <+> prettyTCM tv) $ do
  doVParams p tv $ \ tv' ->
    case tv' of
       VQuant Pi x dom fv | isVSize (typ dom) ->
          underAbs x dom fv $ \ k xv bv -> do
            szSizeVarUsage n co p k bv
       _ -> throwErrorMsg $ "not a valid sized constructor: expected size quantification"

szSizeVarUsage :: Name -> Co -> Int -> Int -> TVal -> TypeCheck ()
szSizeVarUsage n co p i tv = enterDoc (text "szSizeVarUsage of" <+> prettyTCM (VGen i) <+> text "in" <+> prettyTCM tv) $
    case tv of
       VQuant Pi x dom fv -> do
          let av = typ dom
          szSizeVarDataArgs n p i av  -- recursive calls of for D..i..
          enterDoc (text "checking" <+> prettyTCM av <+> text (" to be " ++
              (if co == CoInd then "antitone" else "isotone") ++ " in variable")
              <+> prettyTCM (VGen i)) $
            szMono co i av                -- monotone in i
          underAbs x dom fv $ \ _ xv bv -> do
            szSizeVarUsage n co p i bv

       _ -> szSizeVarTarget p i tv

-- check that Target is of form D ... (Succ i) ...
szSizeVarTarget :: Int -> Int -> TVal -> TypeCheck ()
szSizeVarTarget p i tv = enterDoc (text "szSizeVarTarget, variable" <+> prettyTCM (VGen i) <+> text ("argument no. " ++ show p ++ " in") <+> prettyTCM tv) $ do
    let err = text "expected target" <+> prettyTCM tv <+> text "of size" <+> prettyTCM (VSucc (VGen i))
    case tv of
       VSing _ tv -> szSizeVarTarget p i =<< whnfClos tv
       VApp d vl -> do
               v0 <- whnfClos (vl !! p)
               case v0 of
                 (VSucc (VGen i')) | i == i' -> return ()
                 _ -> failDoc err
       _ -> failDoc err


-- check that rec. arguments are of form D ... i ....
-- and size used nowhere else ?? -- Andreas, 2009-11-27 TOO STRICT!
{- accepts, for instance

   Nat -> Ord i      as argument of a constructor of  Ord ($ i)
   List (Rose A i)   as argument of a constructor of  Rose A ($i)
 -}
szSizeVarDataArgs :: Name -> Int -> Int -> TVal -> TypeCheck ()
szSizeVarDataArgs n p i tv = enterDoc (text "sizeVarDataArgs" <+> prettyTCM (VGen i) <+> text "in" <+> prettyTCM tv) $ do
   case tv of

     {- case D pars sizeArg args -}
     VApp (VDef (DefId DatK (QName m))) vl | n == m -> do
        let (pars, v0 : idxs) = splitAt p vl
        v0 <- whnfClos v0
        case v0 of
          VGen i' | i' == i -> do
            forM_ (pars ++ idxs) $ \ v -> nocc [i] v >>= do
              boolToErrorDoc $
                text "variable" <+> prettyTCM (VGen i) <+>
                text "may not occur in" <+> prettyTCM v
          _ -> failDoc $
                text "wrong size index" <+> prettyTCM v0 <+>
                text "at recursive occurrence" <+> prettyTCM tv

-- not necessary: check for monotonicity above
--     {- case D' pars sizeArg args -}
--     VApp (VDef m) vl | n /= m -> do

     VApp v1 vl -> mapM_ (\ v -> whnfClos v >>= szSizeVarDataArgs n p i) (v1:vl)

     VQuant Pi x dom fv -> do
       szSizeVarDataArgs n p i (typ dom)
       underAbs x dom fv $ \ _ xv bv -> do
          szSizeVarDataArgs n p i bv

     fv | isFun fv ->
       addName (absName fv) $ \ xv -> szSizeVarDataArgs n p i =<< app fv xv
{-
     VLam x env b ->
       addName x $ \ xv -> do
         bv <- whnf (update env x xv) b
         szSizeVarDataArgs n p i bv
-}
     _ -> return ()

{- REMOVED, 2009-11-28, replaced by monotonicity check
     VGen i' -> return $ i' /= i
     VSucc tv' -> szSizeVarDataArgs n p i tv'
 -}

-- doVParams number_of_params constructor_or_datatype_signature
-- skip over parameters of type signature of a constructor/data type
doVParams :: Int -> TVal -> (TVal -> TypeCheck a) -> TypeCheck a
doVParams 0 tv k = k tv
doVParams p (VQuant Pi x dom fv) k =
  underAbs x dom fv $ \ _ xv bv -> do
    doVParams (p - 1) bv k

--------------------------------------
-- check for admissible  type

{-

 - admissibility needs to be check clausewise, because of Karl's example

   fun nonAdmissibleType : Unit -> Set

   fun diverge : (u : Unit) -> nonAdmissibleType u
   {
     diverge unit patterns = badRhs
   }

 - the type must be admissible in the current position
   only if the size pattern is a successor.
   If the pattern is a variable, then there is no induction on that size
   argument, so no limit case, so no upper semi-continuity necessary
   for the type.

 - when checking

     ... (s i) ps  admissible  (j : Size) -> A

   we will check

     A  admissible in j

   and continue with

     ... ps  admissible  A[s i / j]

   just to maintain type wellformedness.  The (s i) in A does not
   really matter, since there is no case distinction on ordinals.

 - a size pattern which is not inductive (meaning there is an
    inductive type indexed by that size) nor coinductive (meaning that
    the result type is coinductive and is indexed by that size) must
    be flagged unusable for termination checking.

 - the fun/cofun distinction could be inferred by the termination checker
   or be clausewise as in Agda 2

-}


admFunDef :: Co -> [Clause] -> TVal -> TypeCheck [Clause]
admFunDef co cls tv = do
  (cls, inco) <- admClauses cls tv
  when (co==CoInd && not (co `elem` inco)) $
    throwErrorMsg $ show tv ++ " is not a type of a cofun" -- ++ if co==Ind then "fun" else "cofun"
  return cls

admClauses :: [Clause] -> TVal -> TypeCheck ([Clause], [Co])
admClauses [] tv = return ([], [])
admClauses (cl:cls) tv = do
  (cl',inco) <- admClause cl tv
  (cls',inco') <- admClauses cls tv
  return (cl' : cls', inco ++ inco')

admClause :: Clause -> TVal -> TypeCheck (Clause, [Co])
admClause (Clause tel ps e) tv = traceAdm ("admClause: admissibility of patterns " ++ show ps) $
   introPatterns ps tv $ \ pvs _ -> do
       (ps', inco) <- admPatterns pvs tv
       return (Clause tel ps' e, inco)

admPatterns :: [(Pattern,Val)] -> TVal -> TypeCheck ([Pattern], [Co])
admPatterns [] tv = do
  isCo <- endsInCo tv
  return ([], if isCo then [CoInd] else [])
admPatterns ((p,v):pvs) tv = do
   (p, inco1)  <- admPattern p tv
   bv <- piApp tv v
   (ps, inco2) <- admPatterns pvs bv
   return (p:ps, inco1 ++ inco2)

{-
-- turn a pattern into a value
-- extend delta by generic values but do not introduce their types
evalPat :: Pattern -> (Val -> TypeCheck a) -> TypeCheck a
evalPat p f =
    case p of
      VarP n -> addName n f
      ConP co n [] -> f (VCon co n)
      ConP co n pl -> evalPats pl $ \ vl -> f (VApp (VCon co n) vl)
      SuccP p -> evalPat p $ \ v -> f (VSucc v)
-- DOES NOT WORK SINCE e has unbound variables
      DotP e -> do
        v <- whnf' e
        f v

evalPats :: [Pattern] -> ([Val] -> TypeCheck a) -> TypeCheck a
evalPats [] f = f []
evalPats (p:ps) f = evalPat p $ \ v -> evalPats ps $ \ vs -> f (v:vs)
-}

{-
evalPat :: Pattern -> TypeCheck (State TCContext Val)
evalPat p =
    case p of
      VarP n -> return $ State $ \ ce ->
        let (k, delta) = cxtPushGen (context ce)
            rho = update n (VGen k) (environ ce)
        in  (VGen k, TCContext { context = delta, environ = rho })
      ConP co n [] -> return (VCon co n)
      ConP co n pl -> do
        vl <- mapM evalPat pl
        return (VApp (VCon co n) vl)
      SuccP p -> do
       v <- evalPat p
       return (VSucc v)
-- TODO: does not work!
--      DotP e -> return $ State $ \ ce ->
-}




{- 2013-03-31 On instantiation of quantifiers [i < #] - F i

If F is upper semi-continuous then

  [i < #] -> F i   is a sub"set" of   F #

so we can instantiate i to #.  (Hughes et al., POPL 96; Abel, LMCS 08)

1) Consider the special case

  F i = [j < i] -> G i

Because # is a limit, thus, j < i < #  iff j < #, we reason:

  F # = [j < #] -> G j

  [i < #] -> F i
      = [i < #] -> [j < i] -> G j  (since # is a limit)
      = [j < #] -> G j

2) Consider the special case

  F i = [j <= i] -> G j

We have

  F # = [j <= #] -> G j
      = G # /\ ([j < #] -> G j)

  [i < #] -> F i
      = [i < #] -> [j <= i] -> G j
      = [j < #] -> G j

So if G is upper semi-continuous, so is F.

-}


-- | Check whether a type is upper semi-continuous.
lowerSemiCont :: Int -> TVal -> TypeCheck Bool
lowerSemiCont i tv = errorToBool $ lowerSemiContinuous i tv

docNotLowerSemi :: Int -> TVal -> TypeCheck Doc
docNotLowerSemi i av = text "type " <+> prettyTCM av <+>
  text " not lower semi continuous in " <+> prettyTCM (VGen i)

lowerSemiContinuous :: Int -> TVal -> TypeCheck ()
lowerSemiContinuous i av = do
  av <- force av
  let fallback = szAntitone i av `newErrorDoc` docNotLowerSemi i av

  case av of

    -- [j < i] & F j  is lower semi-cont in i
    -- because [i < #] & [j < i] & F j is the same as [j < #] & F j
    -- [but what if i in FV(F j)? should not matter!] 2013-04-01
    VQuant Sigma x dom@Domain{ typ = VBelow Lt (VGen i') } fv | i == i' -> return ()

    -- [j <= i] & F j  is lower semi-cont in i if F is
    VQuant Sigma x dom@Domain{ typ = VBelow Le (VGen i') } fv | i == i' -> do
      underAbs x dom fv $ \ j xv bv -> lowerSemiContinuous j bv

    -- Sigma-type general case
    VQuant Sigma x dom@Domain{ typ = av } fv -> do
      lowerSemiContinuous i av
      underAbs x dom fv $ \ _ xv bv -> lowerSemiContinuous i bv

    VApp (VDef (DefId DatK n)) vl -> do
      sige <- lookupSymbQ n
      case sige of

        -- finite tuple type
        DataSig { symbTyp = dv, constructors = cis, isTuple = True } -> do
          -- match target of constructor against tv to instantiate
          --  c : ... -> D ps  -- ps = snd (cPatFam ci)
          mrhoci <- Util.firstJustM $ map (\ ci -> fmap (,ci) <$> nonLinMatchList False emptyEnv (snd $ cPatFam ci) vl dv) cis
          case mrhoci of
            Nothing -> fallback
            Just (rho,ci) -> if (cRec ci) then fallback else do
              -- infinite tuples (recursive constructor) are not lower semi cont
              enter ("lowerSemiContinuous: detected tuple type, checking components") $
                allComponentTypes (cFields ci) rho (lowerSemiContinuous i)

       -- i-sized inductive types are lower semi-cont in i
        DataSig { numPars, isSized = Sized, isCo = Ind } | length vl > numPars -> do
          s <- whnfClos $ vl !! numPars -- the size argument is the first fgter the parameters
          case s of
            VGen i' | i == i' -> return ()
            _ -> fallback

        -- finite inductive type
        DataSig { symbTyp = dv, constructors = cis, isCo = Ind } ->
          -- if any cRec cis then fallback else do -- we loop on recursive data, so exclude
          -- check that we do not loop on the same data names...
          ifM ((n `elem`) <$> asks callStack) fallback $ do
          local (\ ce -> ce { callStack = n : callStack ce }) $ do
          -- match target of constructor against tv to instantiate
          --  c : ... -> D ps  -- ps = snd (cPatFam ci)
          forM_ cis $ \ ci -> do
            match <- nonLinMatchList False emptyEnv (snd $ cPatFam ci) vl dv
            Foldable.forM_ match $ \ rho -> do
                enter ("lowerSemiContinuous: detected tuple type, checking components") $
                  allComponentTypes (cFields ci) rho (lowerSemiContinuous i)

        _ -> fallback
    _ -> fallback

-- | Check whether a type is upper semi-continuous.
upperSemiCont :: Int -> TVal -> TypeCheck Bool
upperSemiCont i tv = errorToBool $ endsInSizedCo' False i tv
  -- 2013-03-30
  -- endsInSizedCo needs tv[0/i] = Top
  -- upperSemiCont does not need this, the target can also be constant in i

-- | @endsInSizedCo i tv@ checks that @tv@ is lower semi-continuous in @i@
--   and that @tv[0/i] = Top@.
endsInSizedCo :: Int -> TVal -> TypeCheck ()
endsInSizedCo = endsInSizedCo' True

-- | @endsInSizedCo' False i tv@ checks that @tv@ is lower semi-continuous in @i@.
--   @endsInSizedCo' True i tv@ additionally checks that @tv[0/i] = Top@.
endsInSizedCo' :: Bool -> Int -> TVal -> TypeCheck ()
endsInSizedCo' endInCo i tv  = enterDoc (text "endsInSizedCo:" <+> prettyTCM tv) $ do
   tv <- force tv
   let fallback
         | endInCo = failDoc $ text "endsInSizedCo: target" <+> prettyTCM tv <+> text "of corecursive function is neither a CoSet or codata of size" <+> prettyTCM (VGen i) <+> text "nor a tuple type"
         | otherwise = szMonotone i tv
   case tv of
      VSort (CoSet (VGen i)) -> return ()
      VMeasured mu bv -> endsInSizedCo' endInCo i bv

      -- case forall j <= i. C j coinductive in i
      VQuant Pi x dom@Domain{ typ = VBelow Le (VGen i') } fv | i == i' ->
        underAbs x dom fv $ \ j xv bv ->
          endsInSizedCo' endInCo j bv
      VGuard (Bound Le (Measure [VGen j]) (Measure [VGen i'])) bv | i == i' ->
        endsInSizedCo' endInCo j bv

      -- same case again, written as j < i+1. C j
      VQuant Pi x dom@Domain{ typ = VBelow Lt (VSucc (VGen i')) } fv | i == i' ->
        underAbs x dom fv $ \ j xv bv ->
          endsInSizedCo' endInCo j bv
      VGuard (Bound Lt (Measure [VGen j]) (Measure [VSucc (VGen i')])) bv | i == i' ->
        endsInSizedCo' endInCo j bv

      -- case forall j < i. C j:  already coinductive in i !!
      -- Trivially, forall j < 0. C j is the top type.
      -- And, forall i < # forall j < i  is equivalent to forall j < #
      -- so we can instantiate i to #.
      VGuard (Bound Lt (Measure [VGen j]) (Measure [VGen i'])) bv | i == i' ->
        return ()
      VQuant Pi x dom@Domain{ typ = VBelow Lt (VGen i') } fv | i == i' -> return ()

      VQuant Pi x dom fv -> do
         lowerSemiContinuous i $ typ dom
         underAbs x dom fv $ \ _ xv bv -> endsInSizedCo' endInCo i bv

      VSing _ tv -> endsInSizedCo' endInCo i =<< whnfClos tv
      VApp (VDef (DefId DatK n)) vl -> do
         sige <- lookupSymbQ n
         case sige of
            DataSig { numPars = np, isSized = Sized, isCo = CoInd }
              | length vl > np -> do
                 v <- whnfClos $ vl !! np
                 if isVGeni v then return () else fallback
                   where isVGeni (VGen i) = True
                         isVGeni (VPlus vs) = and $ map isVGeni vs
                         isVGeni (VMax vs)  = and $ map isVGeni vs
                         isVGeni VZero = True
                         isVGeni _ = False
{- WE DO NOT HAVE SUBST ON VALUES!
                 case vl !! np of
                   VGen j -> if i == j then return () else fail1
                   VZero -> return ()
                   VClos rho e -> do
                     v <- whnf (update rho i VZero) e -- BUGGER
                     if v == VZero then return () else fail1
-}
-- we also allow the target to be a tuple if all of its components
-- fulfill "endsInSizedCo"
            DataSig { symbTyp = dv, constructors = cis, isTuple = True } -> do
              allTypesOfTuple tv vl dv cis (endsInSizedCo' endInCo i)
{-
              -- match target of constructor against tv to instantiate
              --  c : ... -> D ps  -- ps = snd (cPatFam ci)
              mrhoci <- Util.firstJustM $ map (\ ci -> fmap (,ci) <$> nonLinMatchList False emptyEnv (snd $ cPatFam ci) vl dv) cis
              case mrhoci of
                Nothing -> failDoc $ text "endsInSizedCo: panic: target type" <+> prettyTCM tv <+> text "is not an instance of any constructor"
                Just (rho,ci) -> enter ("endsInSizedCo: detected tuple target, checking components") $
                  fieldsEndInSizedCo endInCo i (cFields ci) rho
-}
            _ -> fallback
      _ -> fallback
{- failDoc $ text "endsInSizedCo: target" <+> prettyTCM tv <+> text "of corecursive function is neither a function type nor a codata nor a tuple type"
-}

-- | @allTypesOfTyples args dv cis check@ performs @check@ on all component
--   types of tuple type @tv = d args@ where @dv@ is the type of @d@.
allTypesOfTuple :: TVal -> [Val] -> TVal -> [ConstructorInfo] -> (TVal -> TypeCheck ()) -> TypeCheck ()
allTypesOfTuple tv vl dv cis check = do
  -- match target of constructor against tv to instantiate
  --  c : ... -> D ps  -- ps = snd (cPatFam ci)
  mrhoci <- Util.firstJustM $
    map (\ ci -> fmap (,ci) <$> nonLinMatchList False emptyEnv (snd $ cPatFam ci) vl dv) cis
  -- we know that only one constructor can match, otherwise it would not be a tuple type
  case mrhoci of
    Nothing -> failDoc $ text "allTypesOfTuple: panic: target type" <+> prettyTCM tv <+> text "is not an instance of any constructor"
    Just (rho,ci) -> enter ("allTypesOfTuple: detected tuple target, checking components") $
      allComponentTypes (cFields ci) rho check

{-
fieldsEndInSizedCo :: Bool -> Int -> [FieldInfo] -> Env -> TypeCheck ()
fieldsEndInSizedCo endInCo i fis rho0 = allComponentTypes fis rho0 (endsInSizedCo' endInCo i)
fieldsEndInSizedCo endInCo i fis rho0 = enter ("fieldsEndInSizedCo: checking fields of tuple type " ++ show fis ++ " in environment " ++ show rho0) $
  loop fis rho0 where
    loop [] rho = return ()
    -- nothing to check for erased index fields
    loop (f : fs) rho | fClass f == Index && erased (fDec f) =
      loop fs rho
    loop (f : fs) rho | fClass f == Index = do
      tv <- whnf rho (fType f)
      endsInSizedCo' endInCo i tv
      loop fs rho
    loop (f : fs) rho = do
      tv <- whnf rho (fType f)
      when (not $ erased (fDec f)) $ endsInSizedCo' endInCo i tv
      -- for non-index fields, value is not given by matching, so introduce
      -- generic value
      new (fName f) (Domain tv defaultKind (fDec f)) $ \ xv -> do
        let rho' = update rho (fName f) xv
        -- do not need to check erased fields?
        loop fs rho'
-}

-- | @allComponentTypes fis env check@ applies @check@ to all field types
--   in @fis@ (evaluated wrt to environment @env@).
--   Erased fields are skipped.  (Is this correct?)
allComponentTypes :: [FieldInfo] -> Env -> (TVal -> TypeCheck ()) -> TypeCheck ()
allComponentTypes fis rho0 check = enter ("allComponentTypes: checking fields of tuple type " ++ show fis ++ " in environment " ++ show rho0) $
  loop fis rho0 where
    loop [] rho = return ()

    -- nothing to check for erased index fields
    loop (f : fs) rho | fClass f == Index && erased (fDec f) =
      loop fs rho

    -- ordinary index field types are checked
    loop (f : fs) rho | fClass f == Index = do
      check =<< whnf rho (fType f)
      loop fs rho

    -- proper fields
    loop (f : fs) rho = do
      tv <- whnf rho (fType f)
      -- do not need to check erased fields?
      when (not $ erased (fDec f)) $ check tv
      -- for non-index fields, value is not given by matching, so introduce
      -- generic value
      new (fName f) (Domain tv defaultKind (fDec f)) $ \ xv -> do
        loop fs $ update rho (fName f) xv



endsInCo :: TVal -> TypeCheck Bool
endsInCo tv  = -- traceCheck ("endsInCo: " ++ show tv) $
   case tv of
      VQuant Pi x dom fv -> underAbs x dom fv $ \ _ _ bv -> endsInCo bv

      VApp (VDef (DefId DatK n)) vl -> do
         sige <- lookupSymbQ n
         case sige of
            DataSig { isCo = CoInd } -> -- traceCheck ("found non-sized coinductive target") $
               return True
            _ -> return False
      _ -> return False

-- precondition: Pattern does not contain "Unusable"
admPattern :: Pattern -> TVal -> TypeCheck (Pattern, [Co])
admPattern p tv = traceAdm ("admPattern " ++ show p ++ " type: " ++ show tv) $
  case tv of
      VGuard beta bv -> addBoundHyp beta $ admPattern p bv
      VApp (VDef (DefId DatK d)) vl -> do
         case p of
           ProjP n -> return (p, [])
           _ -> throwErrorMsg "admPattern: IMPOSSIBLE: non-projection pattern for record type"
      VQuant Pi x dom fv -> underAbs x dom fv $ \ k xv bv -> do
  {-
         if p is successor pattern
         check that bv is admissible in k, returning subset of [Ind, CoInd]
         p is usable if either CoInd or it is a var or dot pattern and Ind
-}
         if isSuccessorPattern p then do
           inco <- admType k bv
           when (CoInd `elem` inco && not (shallowSuccP p)) $ cannotMatchDeep p tv
           if (CoInd `elem` inco)
              || (inco /= [] && completeP p)
            then return (p, inco)
            else return (UnusableP p, inco)
          else return (p, [])

      _ -> throwErrorMsg "admPattern: IMPOSSIBLE: pattern for a non-function type"

cannotMatchDeep :: Pattern -> TVal -> TypeCheck ()
cannotMatchDeep p tv = recoverFailDoc $
  text "cannot match against deep successor pattern"
    <+> text (show p) <+> text "at type" <+> prettyTCM tv

admType :: Int -> TVal -> TypeCheck [Co]
admType i tv = enter ("admType: checking " ++ show tv ++ " admissible in v" ++ show i) $
    case tv of
       VQuant Pi x dom@(Domain av _ _) fv -> do
          isInd <- szUsed Ind i av
          when (not isInd) $
            szAntitone i av `newErrorDoc` docNotLowerSemi i av
          underAbs x dom fv $ \ gen _ bv -> do
            inco <- admType i bv
            if isInd then return (Ind : inco) else return inco
       _ -> do
          isCoind <- szUsed CoInd  i tv
          if isCoind then return [CoInd]
           else do
            szMonotone i tv
            return []

szUsed :: Co -> Int -> TVal -> TypeCheck Bool
szUsed co i tv = traceAdm ("szUsed: " ++ show tv ++ " " ++ show co ++ " in v" ++ show i) $
    case tv of
         (VApp (VDef (DefId DatK n)) vl) -> do
                traceAdmM ("szUsed: case data type: " ++ show n ++ show vl)
                sige <- lookupSymbQ n
                case sige of
                  DataSig { numPars = p
                          , isSized = Sized
                          , isCo = co' } | co == co' && length vl > p ->
                      -- p is the number of parameters
                      -- it is also the index of the size argument
                      do s <- whnfClos $ vl !! p
                         case s of
                           VGen i' | i == i' -> return True
                           _ -> return False
                  _ -> return False
         _ -> return False



-- for inductive fun, and for every size argument i
-- - every argument needs to be either inductive or antitone in i
-- - the result needs to be monotone in i

{- szCheckIndFun admpos delta tv

 entry point for admissibility check for recursive functions
 - scans for the first size quantification
 - passes on to szCheckIndFunSize
 - currently: also continues to look for the next size quantification...
 -}

szCheckIndFun :: [Int] -> TVal -> TypeCheck ()
szCheckIndFun admpos tv = -- traceCheck ("szCheckIndFun: " ++ show delta ++ " |- " ++ show tv ++ " adm?") $
      case tv of
       VQuant Pi x dom fv -> underAbs x dom fv $ \ k _ bv -> do
         -- bv <- whnf' b
         if isVSize (typ dom) then do
             when (k `elem` admpos) $
               szCheckIndFunSize k bv
             szCheckIndFun admpos bv -- this is for lexicographic induction on sizes, I suppose?  Probably should me more fine grained!  Andreas, 2008-12-01
          else szCheckIndFun admpos bv
       _ -> return ()


{- szCheckIndFunSize delta i tv

 checks whether type tv is admissible for recursion in index i
 - every argument needs to be either inductive or antitone in i
 - the result needs to be monotone in i
 -}

szCheckIndFunSize :: Int -> TVal -> TypeCheck ()
szCheckIndFunSize i tv = -- traceCheck ("szCheckIndFunSize: " ++ show delta ++ " |- " ++ show tv ++ " adm(v" ++ show i ++ ")?") $
    case tv of
       VQuant Pi x dom fv -> do
            szLowerSemiCont i (typ dom)
--            new x dom $ \ k _  -> szCheckIndFunSize i =<< app fv (VGen k)
            underAbs x dom fv $ \ _ _ bv -> szCheckIndFunSize i bv
{-
            new' x dom $ do
              bv <- whnf' b
              szCheckIndFunSize i bv
-}
       _ -> szMonotone i tv

{- szLowerSemiCont

 - check for lower semi-continuity [Abel, CSL 2006]
 - current approximation: inductive type or antitone
 -}
szLowerSemiCont :: Int -> TVal -> TypeCheck ()
szLowerSemiCont i av = -- traceCheck ("szlowerSemiCont: checking " ++ show av ++ " lower semi continuous in v" ++ show i) $
   (szAntitone i av `catchError`
      (\ msg -> -- traceCheck (show msg) $
                   szInductive i av))
        `newErrorDoc` docNotLowerSemi i av


{- checking cofun-types for admissibility

conditions:

1. type must end in coinductive type or in sized coinductive type
   indexed by just a variable i which has been quantified in the type

2. in the second case, each argument must be inductive or antitone in i
   optimization:
     arguments types before the quantification over i can be ignored
-}

data CoFunType
  = CoFun             -- yes, but not sized cotermination
  | SizedCoFun Int    -- yes an admissible sized type (the Int specifies the number of the recursive size argument)

{-
design:

admCoFun delta tv : IsCoFunType

   endsInCo delta tv (len delta) id

admEndsInCo delta tv firstVar jobs : IsCoFunType

   traverse tv, gather continutations in jobs, check for CoInd in the end

   if tv = (x:A) -> B
      push A on delta
      add the following task to jobs:
        check A for lower semicontinuity in delta
      continue on B

   if tv = Codata^i
      run (jobs i)
      if they return (), return YesSized Int, otherwise No

   if tv = Codata
      return Yes

   otherwise
      return No
 -}

-- {- TODO: FINISH THIS!!

admCoFun :: TVal -> TypeCheck CoFunType
admCoFun tv = do
  l <- getLen
  admEndsInCo tv l (\ i -> return ())

admEndsInCo :: TVal -> Int -> (Int -> TypeCheck ()) -> TypeCheck CoFunType
admEndsInCo tv firstVar jobs = -- traceCheck ("admEndsInCo: " ++ show tv) $
   case tv of
      VQuant Pi x dom fv -> do
         l <- getLen
         let jobs' = (addJob l (typ dom) jobs)
         underAbs x dom fv $ \ _ _ bv -> admEndsInCo bv firstVar jobs'
{-
         new' x dom $ do
           bv <- whnf' b
           admEndsInCo bv firstVar jobs'
-}

{-
      -- if not applied, it cannot be a sized type
      VDef n -> do
         sig <- gets signature
         case (lookupSig n sig) of
            DataSig { isCo = CoInd } -> -- traceCheck ("found non-sized coinductive target") $
               return CoFun
            _ -> throwErrorMsg $ "type of cofun does not end in coinductive type"
-}

      VApp (VDef (DefId DatK n)) vl -> do
         sige <- lookupSymbQ n
         case sige of
            DataSig { isSized = NotSized, isCo = CoInd } -> -- traceCheck ("found non-sized coinductive target") $
               return CoFun
            DataSig { numPars = p, isSized = Sized, isCo = CoInd } | length vl > p -> -- traceCheck ("found sized coinductive target") $
              do
               -- p is the number of parameters
               -- it is also the index of the size argument
               s <- whnfClos $ vl !! p
               case s of
                  VGen i -> do
                     jobs i
                     return $ SizedCoFun $ i - firstVar
                  _ -> throwErrorMsg $ "size argument in result type must be a variable"
            _ -> throwErrorMsg $ "type of cofun does not end in coinductive type"

addJob :: Int -> TVal -> (Int -> TypeCheck ())
       -> (Int -> TypeCheck ())
addJob l tv jobs recVar = do
  -- is the "recursive" size variable actually in scope?
  jobs recVar
  when (recVar < l) $ szLowerSemiCont recVar tv

-- -}


{- szCheckCoFun  OBSOLETE!!

 entry point for admissibility check for corecursive functions
 - scans for the first size quantification
 - passes on to szCheckIndFunSize
 - currently: also continues to look for the next size quantification
 - and checks in the end whether the target is a coinductive type


-- STALE COMMENT: for a cofun : arguments nocc i and result coinductive in i
szCheckCoFun :: SemCxt -> TVal -> TypeCheck ()
szCheckCoFun delta tv =
      case tv of
       VPi dec x av env b -> do
                let (k, delta') = cxtPush dec av delta
                bv <- whnf (update env x (VGen k)) b
                case av of
                  VSize -> do szCheckCoFunSize delta' k bv
                              szCheckCoFun delta' bv
                  _ -> szCheckCoFun delta' bv
       -- result
       (VApp (VDef n) vl) ->
          do sig <- gets signature
             case (lookupSig n sig) of
               (DataSig _ _ _ CoInd _) ->
                   return ()
               _ -> throwErrorMsg $ "cofun doesn't target coinductive type"
       (VDef n)  ->
          do sig <- gets signature
             case (lookupSig n sig) of
               (DataSig _ _ _ CoInd _) ->
                   return ()
               _ -> throwErrorMsg $ "cofun doesn't target coinductive type"
       _ -> throwErrorMsg $ "cofun doesn't target coinductive type"

szCheckCoFunSize :: SemCxt -> Int -> TVal -> TypeCheck ()
szCheckCoFunSize delta i tv = -- traceCheck ("szco " ++ show tv) $
      case tv of
       VPi dec x av env b ->  do
             let (k, delta') = cxtPush dec av delta
             bv <- whnf (update env x (VGen k)) b
             szLowerSemiCont delta i av
             szCheckCoFunSize delta' i bv
       -- result must be coinductive
       _ -> szCoInductive delta i tv

-}

szMono :: Co -> Int -> TVal -> TypeCheck ()
szMono co i tv =
    case co of
         Ind   -> szMonotone i tv
         CoInd -> szAntitone i tv

szMonotone :: Int -> TVal -> TypeCheck ()
szMonotone i tv = traceCheck ("szMonotone: " -- ++ show delta ++ " |- "
                              ++ show tv ++ " mon(v" ++ show i ++ ")?") $
 do
   let si = VSucc (VGen i)
   tv' <- substitute (sgSub i si) tv
   leqVal Pos vTopSort tv tv'

szAntitone :: Int -> TVal -> TypeCheck ()
szAntitone i tv = traceCheck ("szAntitone: " -- ++ show delta ++ " |- "
                              ++ show tv ++ " anti(v" ++ show i ++ ")?") $
 do
   let si = VSucc (VGen i)
   tv' <- substitute (sgSub i si) tv
   leqVal Neg vTopSort tv tv'

-- checks if tv is a sized inductive type of size i
szInductive :: Int -> TVal -> TypeCheck ()
szInductive i tv = szUsed' Ind i tv

-- checks if tv is a sized coinductive type of size i
szCoInductive :: Int -> TVal -> TypeCheck ()
szCoInductive i tv = szUsed' CoInd i tv

szUsed' :: Co -> Int -> TVal -> TypeCheck ()
szUsed' co i tv =
    case tv of
         (VApp (VDef (DefId DatK n)) vl) ->
             do sige <- lookupSymbQ n
                case sige of
                  DataSig { numPars = p, isSized = Sized, isCo =  co' } | co == co' && length vl > p ->
                      -- p is the number of parameters
                      -- it is also the index of the size argument
                      do s <- whnfClos $ vl !! p
                         case s of
                           VGen i' | i == i' -> return ()
                           _ -> throwErrorMsg $ "expected size variable"
                  _ -> throwErrorMsg $ "expected (co)inductive sized type"
         _ -> throwErrorMsg $ "expected (co)inductive sized type"
