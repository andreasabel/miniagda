{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
      PatternGuards, TupleSections, NamedFieldPuns #-}

module TypeChecker where

import Control.Applicative hiding (Const) -- ((<$>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Traversable as Traversable

import Debug.Trace

import qualified Text.PrettyPrint as PP

import Util
import qualified Util as Util

import Abstract
import Polarity as Pol
import Value
import TCM
import Eval
import Extract
import SPos (nocc)
-- import CallStack
import PrettyTCM
import TraceError

import Warshall -- size constraint checking

import Termination

-- import Completness



traceCheck msg a = a -- trace msg a 
traceCheckM msg = return () -- traceM msg
{-
traceCheck msg a = trace msg a 
traceCheckM msg = traceM msg
-}


traceAdm msg a = a -- trace msg a 
traceAdmM msg = return () -- traceM msg
{-
traceAdm msg a = trace msg a 
traceAdmM msg = traceM msg
-}
  
{- DEAD CODE
runWhnf :: Signature -> TypeCheck a -> IO (Either TraceError (a,Signature))
runWhnf sig tc = (runErrorT (runStateT tc  sig)) 
-} 

doNf sig e = runErrorT (runReaderT (runStateT (whnf emptyEnv e >>= reify) (initWithSig sig)) emptyContext)
doWhnf sig e = runErrorT (runReaderT (runStateT (whnf emptyEnv e >>= whnfClos) (initWithSig sig)) emptyContext)


-- top-level functions -------------------------------------------

runTypeCheck :: TCState -> TypeCheck a -> IO (Either TraceError (a,TCState))
runTypeCheck st tc = runErrorT (runReaderT (runStateT tc st) emptyContext) 
-- runTypeCheck st tc = runCallStackT (runReaderT (runStateT tc st) emptyContext) []
 
typeCheck dl = runTypeCheck initSt (typeCheckDecls dl)

-- checking top-level declarations -------------------------------

echo :: MonadIO m => String -> m ()
echo = liftIO . putStrLn

echoR = echo
-- echoR s = echo $ "R> " ++ s

echoTySig :: MonadIO m => Name -> Expr -> m ()
echoTySig n t = return () -- echo $ "I> " ++ n ++ " : " ++ show t

echoKindedTySig :: MonadIO m => Kind -> Name -> Expr -> m ()
echoKindedTySig ki n t = echo $ prettyKind ki ++ "  " ++ show n ++ " : " ++ show t

echoKindedDef :: MonadIO m => Kind -> Name -> Expr -> m ()
echoKindedDef ki n t = echo $ prettyKind ki ++ "  " ++ show n ++ " = " ++ show t

 
echoEPrefix = "E> "

echoTySigE :: MonadIO m => Name -> Expr -> m ()
echoTySigE n t = echo $ echoEPrefix ++ show n ++ " : " ++ show t

echoDefE :: MonadIO m => Name -> Expr -> m ()
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
  typeCheckDecls ds
  put st             -- forget the effect of these decls
  return []
typeCheckDeclaration (OverrideDecl Fail ds) = do
  st <- get
  r <- (typeCheckDecls ds >> return True) `catchError` 
        (\ s -> do liftIO $ putStrLn ("block fails as expected, error message:\n" ++ show s)
                   return False)
  if r then fail "unexpected success" else do
    put st
    return []

typeCheckDeclaration (OverrideDecl TrustMe ds) = 
  newAssertionHandling Warning $ typeCheckDecls ds

typeCheckDeclaration (OverrideDecl Impredicative ds) = 
  goImpredicative $ typeCheckDecls ds

typeCheckDeclaration (RecordDecl n tel t0 c fields) = 
  -- just one "mutual" declaration
  checkingMutual (Just $ DefId DatK n) $ do
    result <- typeCheckDataDecl n NotSized CoInd [] tel t0 [c] fields
    checkPositivityGraph
    return result

typeCheckDeclaration (DataDecl n sz co pos0 tel t0 cs fields) = 
  -- just one "mutual" declaration
  checkingMutual (Just $ DefId DatK n) $ do
    result <- typeCheckDataDecl n sz co pos0 tel t0 cs fields
    checkPositivityGraph
    return result

typeCheckDeclaration (LetDecl bla (TypeSig n t) e ) = 
   (
    do echoTySig n t -- debugging only
       Kinded ki0 te <- checkType t 
       vt <- whnf' te
       -- getEnv >>= \ rho -> traceCheckM $ "checking let-body " ++ show e ++ " : " ++ show vt ++ " in environment " ++ show rho
       Kinded ki1 ee <- checkExpr e vt
       rho <- getEnv -- is emptyEnv
       -- TODO: solve size constraints
       -- does not work with emptyEnv
       -- [te, ee] <- solveAndModify [te, ee] rho  -- solve size constraints
       let v = mkClos rho ee -- delay whnf computation
       -- v  <- whnf' ee -- WAS: whnf' e'
       let ki = (intersectKind ki1 $ predKind ki0)
       addSig n (LetSig vt ki v $ undefinedFType n)    -- late (var -> expr) binding, but ok since no shadowing
--       addSig n (LetSig vt e')    -- late (var -> expr) binding, but ok since no shadowing
       echoKindedTySig ki n te
--       echoTySigE n te
--       echoDefE   n ee
       echoKindedDef ki n ee
       return [LetDecl bla (TypeSig n te) ee]
   ) `throwTrace` show n

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
  -- then check bodies
  -- we need to construct a positivity graph
  edss <- addKindedTypeSigs ktss $ 
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
typeCheckMutualSig (LetDecl ev ts e) =  
  typeCheckSignature ts
typeCheckMutualSig (DataDecl n sz co pos tel t cs fields) = do
  Kinded ki ts <- typeCheckSignature (TypeSig n (teleToType tel t))
  return $ Kinded ki ts
typeCheckMutualSig (FunDecl co (Fun ts n' ar cls)) =
  typeCheckSignature ts 
typeCheckMutualSig (OverrideDecl TrustMe [d]) = 
  newAssertionHandling Warning $ typeCheckMutualSig d
typeCheckMutualSig (OverrideDecl Impredicative [d]) =
  goImpredicative $ typeCheckMutualSig d
typeCheckMutualSig d = fail $ "typeCheckMutualSig: panic: unexpected declaration " ++ show d 

-- typeCheckMutualBody measured kindCandidate
typeCheckMutualBody :: Bool -> Kind -> Declaration -> TypeCheck [EDeclaration]
typeCheckMutualBody measured _ (DataDecl n sz co pos tel t cs fields) = do
  -- set name of mutual thing whose body we are checking
  checkingMutual (Just $ DefId DatK n) $
    -- 
    typeCheckDataDecl n sz co pos tel t cs fields
typeCheckMutualBody measured@False ki (FunDecl co fun@(Fun ts@(TypeSig n t) n' ar cls)) = do
  checkingMutual (Just $ DefId FunK n) $ do
    fun' <- typeCheckFunBody co ki fun
    return $ [FunDecl co fun']

typeCheckDataDecl :: Name -> Sized -> Co -> [Pol] -> Telescope -> Type -> [Constructor] -> [Name] -> TypeCheck [EDeclaration]
typeCheckDataDecl n sz co pos0 tel0 t0 cs0 fields = enter (show n) $
 (do -- sig <- gets signature
     let params = length tel0
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
                 pol | pol `elem` [Param,Rec] -> return $ Quant Pi (TBind x $ Domain tSize kSize $ dec { polarity = polsz }) b
                 pol | pol == polsz -> return t0
                 pol -> fail $ "sized type " ++ show n ++ " has wrong polarity annotation " ++ show pol ++ " at Size argument, it should be " ++ show polsz
             t0 -> return t0
           return (params + 1, pos0 ++ [polsz], t)
         NotSized -> return (params, pos0, t0)
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
     -- get the target sort dsof the datatype
     Kinded ki0 (ds, dte) <- checkDataType p' dt -- TODO?: use above code?
     let ki = dataKind ki0
     echoKindedTySig ki n dte
--     echoTySigE n dte
     v <- whnf emptyEnv dte
     Just fkind <- extractKind v
     -- get the updated telescope which contains the kinds
     let (tel, _) = typeToTele' params dte
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
          NoKind -> kType -- no non-rec constructor arguments
          Kind s s' -> Kind (Set Zero) s' -- a data type is always also a type
     echoKindedTySig newki n dte

     -- solve for size variables
     sol <- solveConstraints
     -- TODO: substitute
     resetConstraints     

     -- add destructors only for the constructors that are non-overlapping
     let mkDestr fi = 
          case (fClass fi) of
             Field (Just (ty, arity, cl)) | not (erased $ fDec fi) && not (emptyName $ fName fi) -> 
               let n' = fName fi
                   n  = internal n' 
               in
               [MutualFunDecl False Ind [Fun (TypeSig n ty) n' arity [cl]]]  
             _ -> []
     -- cEtaExp = True means that all field names are present
     -- and constructor is not overlapping with others
     let mkDestrs ci | cEtaExp ci 
            = concat $ map mkDestr (cFields ci)
         mkDestrs ci = []
     let decls = concat $ map mkDestrs cis
     when (not (null decls)) $ 
        traceCheckM $ "generated destructors: " ++ show decls
     declse <- mapM (\ d@(MutualFunDecl False co [Fun (TypeSig n t) n' ar cls]) -> do 
                       echo $ "G> " ++ showFun co ++ " " ++ show n ++ " : " ++ show t
                       echo $ "G> " ++ PP.render (prettyFun n cls)
                       checkingMutual Nothing $ typeCheckDeclaration d) 
                 decls

     -- decide whether to eta-expand at this type
     -- all patterns need to be proper and non-overlapping
     -- at least one constructor needs to be eta-expandable
     let isPatIndFam = all (\ ci -> fst (cPatFam ci) /= NotPatterns && cEtaExp ci) cis 
--                    && not (or overlapList)
     -- do not eta-expand recursive constructors (might not terminate)
     let disableRec ci {-ov-} rec' = ci { cEtaExp  
           =  cEtaExp ci                      -- all destructors present
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
     let (tele, te) = typeToTele' (length tel) dte
     return $ (DataDecl n sz co pos tele te (map valueOf kcse) fields) : concat declse

   ) -- `throwTrace` n  -- in case of an error, add name n to the trace

{- OLD CODE
{- a record type is a data type that fulfills 3 conditions
   1. non-recursive
   2. exactly 1 constructor
   3. constructor carries names for each of its arguments
 -}
     case isPatIndFam params cs of
      Just [(c,ps)] -> do 
        when (t == Set && nonRec && length cs == 1) $ do
         let constrName   = namePart (head cs) 
         let (ctel, core) = typeToTele (typePart (head cs)) 
         let destrNames = map (\ (dec,x,t) -> x) ctel
         let isRec = all (/= "") destrNames
         when isRec $ do 
  --         echo $ "+++ generating record destructors +++"
  {- now, generate destructors
  
  data Sigma (A : Set) (B : A -> Set) : Set
  {
    pair : (fst : A) -> (snd : B fst) -> Sigma A B
  }
  fst : [A : Set] -> [B : A -> Set] -> (p : Sigma A B) -> A
  { fst A B (pair .A .B _fst _snd) = _fst }
  snd : [A : Set] -> [B : A -> Set] -> (p : Sigma A B) -> B (fst p)
  { snd A B (pair .A .B _fst _snd) = _snd }
   -}      
           -- choose a name for the record to destroy
           let recName = "record_of_type_" ++ n
           let parNames = map (\ (_,x,_) -> x) tel
           -- substitute variable "fst" by application "fst A B p"
           let phi x = if x `elem` destrNames 
                         then App (Def x) (map Var (parNames ++ [recName]))
                         else Var x
           let prefix x =  "destructor_argument_" ++ x
           let pattern = ConP co constrName (map (DotP . Var) parNames 
                                          ++ map (VarP . prefix) destrNames)
           let mkDestr (_,x,t) = MutualFunDecl Ind [(TypeSig x (teleToTypeErase tel t'),[clause])] where 
                   t' = (Pi notErased recName core . parSubst phi) t
                   clause = Clause (map VarP parNames ++ [pattern]) (Var (prefix x))
           let decls = map mkDestr ctel
           mapM_ typeCheckDeclaration decls
           modifySig n (\ dataSig -> dataSig { constructors = Just [(c,ps,destrNames)] })
         return ()
      _ -> return ()
   ) `throwTrace` n  -- in case of an error, add name n to the trace
END OLD CODE -}

{-
typeCheckConstructor :: Name -> Sized -> [Pol] -> Telescope -> Constructor -> TypeCheck ()
typeCheckConstructor d sz pos tel (TypeSig n t) = 
   ( 
   do sig <- gets signature 
      let tt = teleToTypeErase tel t
      echoTySig n tt
      let params = length tel 
      let p' = case sz of
                 Sized -> params + 1
                 NotSized -> params
      checkConType [] cxtEmpty p' tt
      let (_,target) = typeToTele tt
      checkTarget d tel target
      vt <- whnf  [] tt
      sposConstructor d 0 pos vt
      addSig n (ConSig vt)
      case sz of 
        Sized ->
            szConstructor d (length tel) vt
        NotSized -> return ()
      return ()
   ) `throwTrace` n
-}

-- returns True if constructor has recursive argument
typeCheckConstructor :: Name -> Type -> Sized -> Co -> [Pol] -> Telescope -> Constructor -> TypeCheck (Bool, Kinded EConstructor)
typeCheckConstructor d dt sz co pos tel (TypeSig n t) = enter ("constructor " ++ show n) $ do
  sig <- gets signature 
  let telE = map (mapDec (const irrelevantDec)) tel -- need kinded tel!!
    -- parameters are erased in types of constructors
  let tt = teleToType telE t
  echoTySig n tt
  let params = length tel 
{-
  let p' = case sz of
             Sized -> params + 1
             NotSized -> params
-}
  -- when checking constructor types,  do NOT resurrect telescope
  --   data T [A : Set] : Set { inn : A -> T A }
  -- should be rejected, since A ~= T A, and T A = T B means A ~=B for arb. A, B!
  -- add data name as spos var, to check positivity
  -- and as NoKind, to compute the true kind from the constructors
  let telWithD = (TBind d $ Domain dt NoKind $ Dec SPos) : tel
  Kinded ki te <- addBinds telWithD $
    checkConType sz t -- do NOT resurrect telescope!!

  -- check target of constructor
  dv <- whnf' dt
  let (argts,target) = typeToTele te
  addBinds telWithD $ addBinds argts $ checkTarget d dv tel target

  -- make type of a constructor a singleton type
--  let mkName i n | null (suggestion n) = n { suggestion = "y" ++ show i }
  let mkName i n | emptyName n = fresh $ "y" ++ show i 
                 | otherwise   = n
  let argns = zipWith mkName [0..] $ map boundName argts
  let argtbs = zipWith (\ n tb -> tb { boundName = n }) argns argts
  let tsing = teleToType argtbs $ 
       Sing (foldl App (con (coToConK co) n) $ map Var argns) 
            target

  let tte = teleToType telE tsing -- te -- DO resurrect here!
  vt <- whnf' tte
  {- old code was more accurate, since it evaluated before checking
     for recursive occurrence. 
  recOccs <- sposConstructor d 0 pos vt -- get recursive occurrences
  -}
  let recOccs = map (\ tb -> d `elem` usedDefs (boundType tb)) argts
  let isRec = foldl (||) False recOccs
  -- fType <- extractType vt -- moved to Extract
  let fType = undefinedFType n
  isSz <- if sz /= Sized then return Nothing else do
    szConstructor d co params vt -- check correct use of sizes
    if co == CoInd then return $ Just $ error "impossible lhs type of coconstructor" else do
    let lte = teleToType telE $ mkConLType params te
    echoKindedTySig kTerm n lte
    ltv <- whnf' lte
    return $ Just ltv
  addSig n (ConSig params isSz recOccs vt d fType)
--  let (tele, te) = typeToTele (length tel) tte -- NOT NECESSARY
  echoKindedTySig kTerm n tte
  -- traceM ("kind of " ++ n ++ "'s args: " ++ show ki) 
--  echoTySigE n tte
  return (isRec, Kinded ki $ TypeSig n te)

{-
typeCheckFuns :: Co -> [(TypeSig,[Clause])] -> TypeCheck ()
typeCheckFuns co funs = do 
    mapM addFunSig funs
    mapM typeCheckFunSig funs
    -- type check and solve size constraints
    -- return clauses with meta vars resolved
    clss <- mapM typeCheckFunClauses funs 
    -- replace old clauses by new ones in funs
    let funs' = zipWith (\(tysig,cls) cls' -> (tysig,cl')) funs clss
    -- check sized types for admissibility
    mapM (admCheckFunSig (map (\ (TypeSig n t, cls) -> n) funs')) funs'
    liftIO $ terminationCheck funs'  
    mapM enableSig funs'      
    return ()
-}

typeCheckMeasuredFuns :: Co -> [Fun] -> TypeCheck [EFun]
typeCheckMeasuredFuns co funs = do 
    -- echo $ show funs
    kfse <- mapM typeCheckFunSig funs -- also erases measure
    -- TODO: use erased type sigs in funs, but retain measure!

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
    let funse = List.zipWith4 Fun 
                  (map valueOf kfse) 
                  (map funExtName funs) 
                  (map funArity funs) 
                  clse
    -- print reconstructed clauses
    mapM_ (\ (Fun (TypeSig n t) n' ar cls) -> do
        -- echoR $ n ++ " : " ++ show t
        echoR $ (PP.render $ prettyFun n cls))
      funse
    -- replace in signature by erased clauses
    zipWithM (enableSig co) (zipWith intersectKind kis $ map kindOf kfse) funse 
    return $ funse

  where
    enableSig :: Co -> Kind -> Fun -> TypeCheck ()
    enableSig co ki (Fun (TypeSig n t) n' ar' cl') = do
      vt <- whnf' t 
      addSig n (FunSig co vt ki ar' cl' True $ undefinedFType n)
      -- add a let binding for external use
      v <- up False (vFun n) vt
      addSig n' (LetSig vt ki v $ undefinedFType n')



-- type check the body of one function in a mutual block
-- type signature is already checked and added to local context
typeCheckFunBody :: Co -> Kind -> Fun -> TypeCheck EFun
typeCheckFunBody co ki0 fun@(Fun ts@(TypeSig n t) n' ar cls0) = do 
    -- echo $ show fun
    addFunSig co $ Kinded ki0 fun
    -- type check and solve size constraints
    -- return clauses with meta vars resolved
    Kinded ki clse <- setCo co $ typeCheckFunClauses fun

    -- check new clauses for admissibility, inserting "unusuable" flags in the patterns where necessary
    -- TODO: proper cleanup, proper removal of admissibility check!
    -- clse <- admCheckFunSig co names ts clse

    -- print reconstructed clauses    
    -- echoR $ n ++ " : " ++ show t
    echoR $ (PP.render $ prettyFun n clse)
    -- replace in signature by erased clauses
    let fune = Fun ts n' ar clse
    enableSig ki fune
    return fune


typeCheckFuns :: Co -> [Fun] -> TypeCheck [EFun]
typeCheckFuns co funs0 = do 
    -- echo $ show funs
    kfse <- mapM typeCheckFunSig funs0
    let kfuns = zipWith (\ (Kinded ki ts) (Fun ts0 n' ar cls) -> Kinded ki (Fun ts n' ar cls)) kfse funs0
    -- zipWithM (addFunSig co) (map kindOf kfse) funs
    mapM (addFunSig co) kfuns
    let funs = map valueOf kfuns
    -- type check and solve size constraints
    -- return clauses with meta vars resolved
    kce <- setCo co $ mapM typeCheckFunClauses funs
    let kis = map kindOf kce
    let clse = map valueOf kce 
    -- get the list of mutually defined function names 
    let names   = map (\ (Fun (TypeSig n t) n' ar cls) -> n) funs
    -- check new clauses for admissibility, inserting "unusuable" flags in the patterns where necessary
    -- TODO: proper cleanup, proper removal of admissibility check!
    clse <- zipWithM (\ (Fun tysig _ _ _) cls' -> admCheckFunSig co names tysig cls') funs clse
    -- replace old clauses by new ones in funs
    let funse = List.zipWith4 Fun 
                  (map valueOf kfse) 
                  (map funExtName funs)
                  (map funArity funs) 
                  clse
--    let funse = zipWith (\(tysig,(ar,cls)) cls' -> (tysig,(ar,cls'))) funs clse
    -- print reconstructed clauses
    mapM_ (\ (Fun (TypeSig n t) n' ar cls) -> do
        -- echoR $ n ++ " : " ++ show t
        echoR $ (PP.render $ prettyFun n cls))
      funse
    terminationCheck funse 
    -- replace in signature by erased clauses
    zipWithM enableSig kis funse 
    return $ funse

addFunSig :: Co -> Kinded Fun -> TypeCheck ()
addFunSig co (Kinded ki (Fun (TypeSig n t) n' ar cl)) = do
    sig <- gets signature 
    vt <- whnf' t -- TODO: PROBLEM for internal extraction (would need te here)
    addSig n (FunSig co vt ki ar cl False $ undefinedFType n) --not yet type checked / termination checked

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
        

{-
-- admCheckFunSig mutualNames (TypeSig thisName thisType, clauses)
admCheckFunSig :: [Name] -> (TypeSig,[Clause]) -> TypeCheck () 
admCheckFunSig mutualNames (TypeSig n t, cls) =   
   (
    do -- a function is not recursive if did does not mention any of the
       -- mutually defined function names
       let usedNames = rhsDefs cls
       let notRecursive = all (\ n -> not (n `elem` usedNames)) mutualNames 
       -- for non-recursive functions, we can skip the admissibility check
       when (not notRecursive) $ do
         vt <- whnf [] t
       when (not notRecursive) $ do
         vt <- whnf [] t              
         (case co of
            Ind -> do 
               when (not (completeFun cls)) $
                  fail $ n ++ " : size pattern incomplete" 
               let admpos = getSizeMatches cls -- admissible in which size variables?  
               szCheckIndFun admpos vt `throwTrace` "type not admissible"

            CoInd -> do 
               res <- admCoFun vt `throwTrace` "type not admissible"
               case res of
                  CoFun -> return ()
                  SizedCoFun i ->
                     when (not (coFunSuccPat i cls)) $
                        fail $ n ++ " : corecursive size arguments can only be matched agains ($ i) for some size variable i"       
            )  -- TODO: this might be incorrect for mutual cofuns
    ) `throwTrace` ("type of " ++ n)
-}

enableSig :: Kind -> Fun -> TypeCheck ()
enableSig ki (Fun (TypeSig n _) n' ar' cl') = do 
  (FunSig co vt ki0 ar cl _ ftyp) <- lookupSymb n
  addSig n (FunSig co vt (intersectKind ki ki0) ar cl' True ftyp)
  -- add a let binding for external use
  v <- up False (vFun n) vt
  addSig n' (LetSig vt ki v ftyp)


-- typeCheckFunSig (TypeSig thisName thisType, clauses)
typeCheckFunSig :: Fun -> TypeCheck (Kinded ETypeSig) 
typeCheckFunSig (Fun (TypeSig n t) n' ar cls) = enter ("type of " ++ show n) $ do  
  echoTySig n t
  Kinded ki0 te0 <- checkType t
  let te = eraseMeasure te0
  let ki = predKind ki0
  echoKindedTySig ki n te
--  echoTySigE n te
  return $ Kinded ki $ TypeSig n te

typeCheckFunClauses :: Fun -> TypeCheck (Kinded [EClause])
typeCheckFunClauses (Fun (TypeSig n t) n' ar cl) = enter (show n) $
   do result@(Kinded _ cle) <- checkFun t cl
      -- traceCheck (show (TypeSig n t)) $
       -- traceCheck (show cl') $
      -- echo $ PP.render $ prettyFun n cle
      return result

{-
checkConType :: Env -> SemCxt -> Int -> Expr -> TypeCheck ()
checkConType rho delta p e =
    case e of
      Pi dec x t1 t2 -> do
             case (len delta) < p of
               True ->
                   return ()
               False ->
                   checkSmallType rho delta t1
             v_t1 <- whnf rho t1
             let (k, delta') = cxtPush (Dec False) v_t1 delta
             checkConType (update rho x (VGen k)) (delta') p t2
      _ -> checkExpr rho delta e VSet
 -}

{-
-- check that arguments are stypes
-- check that result is a set 
--  ( params were already checked by checkDataType ) 
checkConType :: Int -> Expr -> TypeCheck Extr
checkConType p e = 
    case e of
      Pi dec x t1 t2 -> do
             l <- getLen
             when (l >= p) $ checkSmallType t1
             addBind x (Dec False) t1 $ do
               checkConType p t2 
      _ -> checkSmallType e 

-- checkConType numpars t = te
-- check that arguments are stypes
-- check that result is a set 
--  ( params were already checked by checkDataType ) 
-- called initially in the empty context
-- the extracted type is does not include the parameter telescope!
checkConType :: Sized -> Int -> Expr -> TypeCheck Extr
checkConType NotSized 0 t = checkConTypeCore t 
-- checkConType Sized 0 t = checkConTypeCore t 
checkConType Sized 0 t =
    case t of
      Pi dec x (Sort Size) t2 -> do
             addBind x (Dec False) (Sort Size) $ do
               t2e <- checkConTypeCore t2
               return $ Pi dec x (Sort Size) t2e 
      _ -> fail $ "checkConType: internal error, t=" ++ show t

checkConType sz n t =
    case t of
      Pi dec x t1 t2 -> do
             -- no need to check parameter t1, already checked in data decl
             addBind x (Dec False) t1 $ do
               checkConType sz (n-1) t2 
      _ -> fail $ "checkConType: internal error, n=" ++ show n ++ " t=" ++ show t
-}

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
      _ -> fail $ "checkConType: expecting size quantification, found " ++ show t

checkConType' :: Expr -> TypeCheck (Kinded Extr)
checkConType' t = do
  (s, kte) <- checkingCon True $ inferType t
  case s of
    Set{} -> return kte 
    CoSet{} -> return kte
    _ -> fail $ "checkConType: type " ++ show t ++ " of constructor not a universe"
{-
    case t of
      Pi dec x t1 t2 -> do
             t1e <- checkSmallType t1
             addBind x (Dec False) t1 $ do
               t2e <- checkConType' t2
               return $ Pi dec x t1e t2e 
      _ -> checkSmallType t 
 -}       

-- check that the data type and the parameter arguments (written down like declared in telescope) 
-- precondition: target tg type checks in current context
checkTarget :: Name -> TVal -> Telescope -> Type -> TypeCheck ()
checkTarget d dv tel tg = do
  tv <- whnf' tg
  case tv of
    VApp (VDef (DefId DatK n)) vs | n == d -> do
      telvs <- mapM (\ tb -> whnf' (Var (boundName tb))) tel
      enter ("checking datatype parameters in constructor target") $ 
        leqVals' N mixed (One dv) (take (length tel) vs) telvs
      return ()
    _ -> fail $ "constructor should produce something in data type " ++ show d

{- RETIRED (syntactic check)
checkTarget :: Name -> Telescope -> Type -> TypeCheck ()
checkTarget d tel tg =  
    case spineView tg of
      (Def (DefId Dat n), args) | n == d -> checkParams tel (take (length tel) args)
      _ -> throwErrorMsg $ "target mismatch"  ++ show tg

    where checkParams :: Telescope -> [Expr] -> TypeCheck ()
          checkParams [] [] = return ()
          checkParams (tb : tl) ((Var n') : el) | boundName tb == n' 
            = checkParams tl el
          checkParams tl al = throwErrorMsg $ "target param mismatch " ++ 
            d ++ " " ++ show tel ++ " != " ++ show tg ++ "\ncheckParams " ++ show tl ++ " " ++ show al ++ " failed"
-}
                      
-- check that params are types
-- check that arguments are stypes
-- check that target is set
checkDataType :: Int -> Expr -> TypeCheck (Kinded (Sort Expr, Extr))
checkDataType p e = do
  traceCheckM ("checkDataType " ++ show e ++ " p=" ++ show p)
  case e of
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
                

checkSize :: Expr -> TypeCheck Extr
checkSize e = valueOf <$> checkExpr e vSize

checkBelow :: Expr -> LtLe -> Val -> TypeCheck Extr
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

{-
-- entry point 1: possibly resurrects
checkExpr :: Expr -> TVal -> TypeCheck Extr
checkExpr e v = maybeResurrect v $ checkExpr' e v 

maybeResurrect :: TVal -> TypeCheck a -> TypeCheck a
maybeResurrect v = if v == VSet || v == VSize then resurrect else id

-- entry point 2: possibly resurrects
checkExprDec :: Dec -> Expr -> TVal -> TypeCheck Extr
checkExprDec dec e v = do ee <- maybeResurrectDec dec v $ checkExpr' e v  -- ignore erasure
                          return $ ee -- if erased dec then Irr else ee

-- superseded by TCM.applyDec                           
maybeResurrectDec :: Dec -> TVal -> TypeCheck a -> TypeCheck a
maybeResurrectDec dec v = if erased dec || v == VSet || v == VSize then resurrect else id
-}
{-
-- entry point 2: possibly resurrects, adjusts polarities
checkExprDec :: Dec -> Expr -> TVal -> TypeCheck Extr
checkExprDec dec e v = do 
  ee <- maybeResurrectDec dec $ checkExpr' e v  -- ignore erasure
  return $ ee -- if erased dec then Irr else ee

-- entry point 1: redundant
checkExpr :: Expr -> TVal -> TypeCheck Extr
checkExpr e v = checkExpr' e v 

-- checkExpr e tv = Just e' | Nothing
-- instead of e' = Irr return Nothing
-}

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

      (LLet (TBind x (Domain t1 _ dec)) e1 e2,_) ->
          do 
            Kinded kit t1e <- checkType t1
            v_t1 <- whnf' t1
            Kinded ki0 e1e <- applyDec dec $ checkExpr e1 v_t1
            v_e1 <- whnf' e1
            let ki1 = intersectKind ki0 (predKind kit)
            new x (Domain v_t1 ki1 dec) $ \ vx -> do
              addRewrite (Rewrite vx v_e1) [v] $ \ [v'] -> do
                Kinded ki2 e2e <- checkExpr e2 v'
                return $ Kinded ki2 $ LLet (TBind x (Domain t1e ki1 dec)) e1e e2e  -- if e2e==Irr then Irr else LLet n t1e e1e e2e  
-- Dependent let: not checkable in rho;Delta style
--            v_e1 <- whnf rho e1
--            checkExpr (update rho n v_e1) (v_t1 : delta) e2 v  

      (Case (Var x) [Clause _ [SuccP (VarP y)] (Just rhs)], v) -> do
          (tv, _) <- resurrect $ inferExpr (Var x)
          subtype tv vSize
          vx@(VGen i) <- whnf' (Var x)
          endsInSizedCo i v
          let dom = Domain vSize kSize defaultDec
          newWithGen y dom $ \ j vy -> do
            let vp = VSucc vy
            addSizeRel j 1 i $
              addRewrite (Rewrite vx vp) [v] $ \ [v'] -> do
                Kinded ki2 rhse <- checkRHS [] rhs v'
                return $ Kinded ki2 $ Case (Var x) [Clause [TBind y dom] [SuccP (VarP y)] (Just rhse)]

      (Case e cs, _) -> do
          (tv, Kinded ki1 ee) <- inferExpr e
          ve <- whnf' ee
          -- tv' <- sing' ee tv -- DOES NOT WORK
          Kinded ki2 cle <- checkCases ve (arrow tv v) cs
          return $ Kinded ki2 $ Case ee cle

      (_, VGuard beta bv) ->
        addBoundHyp beta $ checkExpr e bv

      (e,v) | inferable e -> do 
          (v2, Kinded ki1 ee) <- inferExpr e
          checkSubtype ee v2 v 
          return $ Kinded ki1 ee

      _ -> checkForced e v

      ) -- >> (trace ("checkExpr successful: " ++ show e ++ ":" ++ show v) $ return ())

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

      -- metavariables must have type size
      (Meta i, _) | isVSize v -> do 
        addMeta ren i
        return $ Kinded kSize $ Meta i 
{-  problem: what to return here
-}

      (Pair e1 e2, VQuant Sigma y dom@(Domain av ki dec) env b) -> do
         Kinded k1 e1 <- applyDec dec $ checkExpr e1 av
         v1 <- whnf' e1
         bv <- whnf (update env y v1) b
         Kinded k2 e2 <- checkExpr e2 bv
         return $ Kinded (unionKind k1 k2) $ Pair e1 e2

      (Record rs, t@(VApp (VDef (DefId DatK d)) vl)) -> do
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
             return $ Kinded k $ Record rs

           
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

      (Lam _ y e1, VQuant Pi x dom env t1) -> do
          rho <- getEnv  -- get the environment corresponding to Gamma
          new y dom $ \ vy -> do
            v_t1 <- whnf (update env x vy) t1
            -- traceCheckM $ "checking " ++ show e1 ++ " : " ++ show v_t1 
            Kinded ki1 e1e <- checkExpr e1 v_t1
            -- the kind of a lambda is the kind of its body  
            return $ Kinded ki1 $ Lam (decor dom) y e1e

{- 
      (Pi dec n t1 t2, VSort Set) ->
          do t1e <- if t1 == Sort Size then return t1 
                     else checkExpr t1 $ VSort Set  -- Size is not a set TODO:proper PTS rules!
             addBind n (Dec False) t1e $ do  -- ignore erasure flag in Pi!
               t2e <- checkExpr t2 $ VSort Set
             -- never prune a Pi
               return $ Pi dec n t1e t2e
 
      (Pi dec n t1 t2, VSort (CoSet vi)) ->
          do t1e <- if t1 == Sort Size then return t1 
                     else checkExpr t1 $ VSort Set  -- Size is not a set TODO:proper PTS rules!
             addBind n (Dec False) t1e $ do  -- ignore erasure flag in Pi!
               t2e <- checkExpr t2 $ VSort $ CoSet $ maybe vi id $ predSize vi
             -- never prune a Pi
               return $ Pi dec n t1e t2e
-}
      (e, VBelow ltle v) -> Kinded kSize <$> checkBelow e ltle v

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
{-
              -- prune sizes
              return $ if e2e==Irr then Irr else Succ e2e
-}
      (e,v) -> do
        case spineView e of

          -- unfold defined patterns
          (h@(Def (DefId (ConK DefPat) c)), es) -> do
             PatSig xs pat _ <- lookupSymb c
             let (xs1, xs2) = splitAt (length es) xs
                 phi x      = maybe (Var x) id $ lookup x (zip xs1 es)
                 body       = parSubst phi (patternToExpr pat)
                 e          = foldr (Lam defaultDec) body xs2
             checkForced e v

          -- check constructor term 
          (h@(Def (DefId (ConK co) c)), es) -> do
             tv <- conType c v
             (kes, dv) <- checkSpine es tv
             let e = foldl App h $ map valueOf kes
             checkSubtype e dv v
             e <- etaExpandPis e dv -- a bit similiar to checkSubtype, which computes a singleton
             return $ Kinded kTerm $ e 

          -- else infer
          _ -> do 
            (v2,kee) <- inferExpr e 
            checkSubtype (valueOf kee) v2 v 
            return kee

-- | Only eta-expand at function types, do not force.
etaExpandPis :: Expr -> TVal -> TypeCheck Expr
etaExpandPis e tv = do
  case tv of
    VQuant Pi x dom env b -> new x dom $ \ xv -> do
      let y = freshen x
      Lam (decor dom) y <$> do
        etaExpandPis (App e (Var y)) =<< whnf (update env x xv) b
    _ -> return e

checkSpine :: [Expr] -> TVal -> TypeCheck ([Kinded Extr], TVal)
checkSpine [] tv = return ([], tv)
checkSpine (e : es) tv = do
  (ke, tv) <- checkApp e tv
  (kes, tv) <- checkSpine es tv
  return (ke : kes, tv)

checkApp :: Expr -> TVal -> TypeCheck (Kinded Extr, TVal)
checkApp e2 v = do
  v <- force v -- if v is a corecursively defined type in Set, unfold!
  case v of
    VQuant Pi x (Domain av _ dec) env b -> do
       (ki, v2,e2e) <- 
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
              v2 <- whnf' e2
              return (ki, v2, e2e)
       bv <- whnf (update env x v2) b
       -- the kind of the application is the kind of its head
       return (Kinded ki $ if erased dec then erasedExpr e2e else e2e, bv)
       -- if e1e==Irr then Irr else if e2e==Irr then e1e else App e1e [e2e]) 
    _ -> throwErrorMsg $ "checking application to " ++ show e2 ++ ": expected function type, found " ++ show v


-- checkSubtype  expr : infered_type <= ascribed_type
checkSubtype :: Expr -> TVal -> TVal -> TypeCheck ()
checkSubtype e v2 v = do
    rho <- getEnv
    traceCheckM $ "computing singleton <" ++ show e ++ " : " ++ show v2 ++ "> in environment " ++ show rho
    v2principal <- sing rho e v2
    traceCheckM $ "subtype checking " ++ show v2principal ++ " ?<= " ++ show v ++ " in environment " ++ show rho
    subtype v2principal v


-- ptsRule s1 s2 = s  if (s1,s2,s) is a valid rule
-- precondition: s1,s2 are proper sorts, i.e., not Size or Tm
ptsRule :: Sort Val -> Sort Val -> TypeCheck (Sort Val)
ptsRule s1 s2 = do
  cxt <- ask
  let parametric = checkingConType cxt  -- are we dealing with a parametric pi?
  let err = "ptsRule " ++ show (s1,s2) ++ " " ++ (if parametric then "(in type of constructor)" else "") ++ ": "
  case (s1,s2) of
    (Set VInfty,_) -> fail $ err ++ "domain too big"
    (Set v1, Set v2) -> 
      if parametric then do 
         leqSize Pos v1 v2 -- when we are checking a constructor, to reject 
         {- data Bad (A : Set) : Set { bad : (A : Set) -> Bad A } -}
         return s2
       else return $ Set $ maxSize [v1,v2] 
    (CoSet v1, Set VZero) -> 
      if parametric then return $ CoSet v1
       else if v1==VInfty then return $ Set VZero
             else fail $ err ++ "domain cannot be sized"
    (CoSet v1, CoSet v2) -> if parametric then do 
                              let v2' = maybe v2 id $ predSize v2
                              case minSize v1 v2 of
                                Just v -> return $ CoSet v
                                Nothing -> fail $ err ++ "min" ++ show (v1,v2) ++ " does not exist"
                            else if v1==VInfty then return $ CoSet $succSize v2 
                                  else fail $ err ++ "domain cannot be sized"
    _ -> return s2
    

-- inferType t = (s, te)
inferType :: Expr -> TypeCheck (Sort Val, Kinded Extr)
inferType t = do
  (sv, te) <- inferExpr t
  case sv of
    VSort s | not (s `elem` map SortC [Tm,Size]) -> return (s,te) 
    _ -> fail $ "inferExpr: expected " ++ show t ++ " to be a type!"
  
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

checkGuard :: Bound Val -> TypeCheck ()
checkGuard beta@(Bound ltle mu mu') = enterDoc (text "checkGuard" <+> prettyTCM beta) $
  lexSizes ltle (measure mu) (measure mu')

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

      Var x -> do 
        -- traceCheckM ("infer variable " ++ x) $ 
        item <- lookupName1 x
        enterDoc (text "inferExpr: variable" <+> prettyTCM x <+> colon <+> prettyTCM (typ $ domain item) <+> text "may not occur") $ do
          let dec = decor $ domain item
          let udec = upperDec item
          let pol = polarity dec
          let upol = polarity udec
          when (erased dec && not (erased udec)) $ 
            recoverFail ", because it is marked as erased"
          enter ", because of polarity" $ 
            leqPolM pol upol
           -- traceCheckM ("infer variable " ++ x ++ " : " ++ show  (typ item))
        return $ (typ $ domain item, Kinded (kind $ domain item) $ Var x)
{-
        let err = "inferExpr: variable " ++ x ++ " : " ++ show (typ item) ++ 
                  " may not occur"
        let dec = decor item
        let pol = polarity dec
        if erased dec then 
          fail $ err ++ ", because it is marked as erased"
         else if not (leqPol pol SPos) then
          fail $ err ++ ", because it has polarity " ++ show pol
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
          fail $ "constructor arguments must be declared mixed-variant"
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
          s <- if impredicative ce && erased dec && s2 == Set VZero then return s2 else ptsRule s1 s2 -- Impredicativity!
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

      App e1 (Proj p) -> checkingCon False $ do
            (v, Kinded ki1 e1e) <- inferExpr e1
{-
            let fail1 = failDoc (text "expected" <+> prettyTCM e1 <+> text "to be of record type when taking the projection" <+> text p <> comma <+> text "but found type" <+> prettyTCM v)
            let fail2 = failDoc (text "record" <+> prettyTCM e1 <+> text "of type" <+> prettyTCM v <+> text "does not have field" <+> text p)
-}
            v <- force v -- if v is a corecursively defined type in Set, unfold!
            tv <- projectType v p
            return (tv, Kinded ki1 (App e1e (Proj p)))
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
              
      App e1 e2 -> checkingCon False $ do
        (v, Kinded ki1 e1e) <- inferExpr e1
        (Kinded ki2 e2e, bv) <- checkApp e2 v
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
      (Def id@(DefId {idKind, name})) | not (conKind idKind) -> do -- traceCheckM ("infer defined head " ++ show n) 
         mitem <- errorToMaybe $ lookupName1 name
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
                 do sige <- lookupSymb name
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
        _ -> fail $ "expected " ++ show s ++ " to be Set or CoSet _"

{-
-- small type
checkSmallType :: Expr -> TypeCheck Extr
checkSmallType e  = (resurrect $ checkExpr' e $ VSort Set) `throwTrace` ("not a set: " ++ show e )
-}

-- check telescope and add bindings to contexts
checkTele :: Telescope -> (ETelescope -> TypeCheck a) -> TypeCheck a
checkTele = checkTele' []

checkTele' :: ETelescope -> Telescope -> (ETelescope -> TypeCheck a) -> TypeCheck a
checkTele' acc [] k = k (reverse acc)
checkTele' acc (tb@(TBind x (Domain t _ dec)) : tel) k = do
  Kinded ki te <- checkType t
  addBind (TBind x (Domain t ki dec)) $
    checkTele' (TBind x (Domain te ki dec) : acc) tel k
 
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
checkCase i v tv cl@(Clause _ [p] mrhs) =
  -- traceCheck ("checking case " ++ show i) $
    (
    do   
      (flex,ins,cxt,vt,pe,pv,absp) <- checkPattern neutral [] [] tv p
      local (\ _ -> cxt) $ do
        mapM (checkGoal ins) flex
        tel <- getContextTele -- TODO!
        case (absp,mrhs) of
           (True,Nothing) -> return $ Kinded NoKind (Clause tel [pe] Nothing) 
           (False,Nothing) -> fail ("missing right hand side in case " ++ showCase cl)
           (True,Just rhs) -> fail ("absurd pattern requires no right hand side in case " ++ showCase cl)
           (False,Just rhs) -> do
              -- pv <- whnf' (patternToExpr p) -- DIFFICULT FOR DOT PATTERNS!
      --        vp <- patternToVal p -- BUG: INTRODUCES FRESH GENS, BUT THEY HAVE ALREADY BEEN INTRODUCED IN checkattern
              addRewrite (Rewrite v pv) [vt] $ \ [vt'] -> do
                Kinded ki rhse <- checkRHS ins rhs vt'
                return $ Kinded ki (Clause tel [pe] (Just rhse))
                -- [rhs'] <- solveAndModify [rhs] (environ cxt)
                -- return (Clause [p] rhs')
    ) `throwTrace` ("case " ++ show i)

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
    (flex,ins,cxt,tv0,ple,plv,absp) <- checkPatterns neutral [] [] tv pl
    local (\ _ -> cxt) $ do
      mapM (checkGoal ins) flex
      -- TODO: insert meta var solution in dot patterns
      tel <- getContextTele -- WRONG TELE, has VGens for DotPs
      case (absp,mrhs) of
         (True,Nothing) -> return $ Kinded NoKind (Clause tel ple Nothing) 
         (False,Nothing) -> fail ("missing right hand side in clause " ++ show cl)
         (True,Just rhs) -> fail ("absurd pattern requires no right hand side in clause " ++ show cl)
         (False,Just rhs) -> do
            Kinded ki rhse <- checkRHS ins rhs tv0
            env  <- getEnv
            [rhse] <- solveAndModify [rhse] env
            return $ Kinded ki (Clause tel ple (Just rhse))

 
-- * Pattern checking ------------------------------------------------

type Substitution = [(Int,TVal)]

type DotFlex = (Int,(Expr,Domain))

-- left over goals
data Goal = DotFlex Int Expr Domain
          | MaxMatches Int TVal 

-- checkPatterns is initially called with an empty local context
-- in the type checking monad
checkPatterns :: Dec -> [Goal] -> Substitution -> TVal -> [Pattern] -> TypeCheck ([Goal],Substitution,TCContext,TVal,[EPattern],[Val],Bool)
checkPatterns dec0 flex ins v pl = 
  case v of
    VMeasured mu vb -> setMeasure mu $ checkPatterns dec0 flex ins vb pl
    VGuard beta vb -> addBoundHyp beta $ checkPatterns dec0 flex ins vb pl
{-
    VGuard beta vb -> fail $ "checkPattern at type " ++ show v ++ " --- introduction of constraints not supported"
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
          tv <- projectType tv proj
          cxt <- ask
          return (flex, ins, cxt, tv, p, VProj proj, False)
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

    -- function type can be eliminated
    VQuant Pi x (Domain av ki dec) env b -> do
{-
       let erased' = er || erased dec
       let decEr   = if erased' then irrelevantDec else dec -- dec {erased = erased'}
-}
       let decEr = dec `compose` dec0
       let domEr   =  (Domain av ki decEr)
       case p of 
         
         -- treat successor pattern here, because of admissibility check
         SuccP p2 -> do  
                 when (av /= vSize) (throwErrorMsg "checkPattern: expected type Size") 

                 when (isSuccessorPattern p2) $
                   fail ("cannot match against deep successor pattern " 
                    ++ show p ++ "; type: " ++ show tv)

                 co <- asks mutualCo
                 when (co /= CoInd) $
                   fail ("successor pattern only allowed in cofun")

                 enterDoc (text ("checkPattern " ++ show p ++" : matching on size, checking that target") <+> prettyTCM tv <+> text "ends in correct coinductive sized type") $
                   newWithGen x domEr $ \ i xv -> do
                     vb0 <- whnf (update env x xv) b
                     endsInSizedCo i vb0

                 cxt <- ask
                 (flex',ins',cxt',tv',p2e,p2v,absp) <- checkPattern decEr flex ins (vSize `arrow` vSize) p2
                 -- leqVal Mixed delta' VSet VSize av -- av = VSize 
                 let pe = SuccP p2e
                 let pv = VSucc p2v
--                 pv0 <- local (\ _ -> cxt') $ whnf' $ patternToExpr pe
                 -- pv0 <- patternToVal p -- RETIRE patternToVal
                 -- pv  <- up False pv0 av -- STUPID what can be eta-exanded at type Size??
                 vb  <- whnf (update env x pv) b
{-
                 endsInCoind <- endsInSizedCo pv vb
                 when (not endsInCoind) $ throwErrorMsg $ "checkPattern " ++ show p ++" : cannot match on size since target " ++ show tv ++ " does not end in correct coinductive sized type"
-}
                 return (flex',ins',cxt',vb,pe,pv,absp) 

         -- other patterns: no need to know about result type
         _ -> do
           (flex',ins',cxt',pe,pv,absp) <- checkPattern' flex ins domEr p
           vb  <- whnf (update env x pv) b
           vb  <- substitute ins' vb  -- from ConP case -- ?? why not first subst and then whnf?
           -- traceCheck ("Returning type " ++ show vb) $
           return (flex',ins',cxt',vb,pe,pv,absp) 

    _ -> throwErrorMsg $ "checkPattern: expected function type, found " ++ show tv
         
-- TODO: refactor with monad transformers
-- put absp into writer monad

checkPattern' :: [Goal] -> Substitution -> Domain -> Pattern -> TypeCheck ([Goal],Substitution,TCContext,EPattern,Val,Bool)
checkPattern' flex ins domEr@(Domain av ki decEr) p = do
       let erased' = erased decEr
       let maybeErase p = if erased' then ErasedP p else p
--       let erasedOrIrrefutable p = if erased' then ErasedP p else IrrefutableP p
       case p of
          SuccP{} -> failDoc (text "successor pattern" <+> prettyTCM p <+> text "not allowed here")

          PairP p1 p2 -> do
            av <- force av
            case av of
             VQuant Sigma y dom1@(Domain av1 ki1 dec1) env1 a2 -> do
              (flex, ins, cxt, pe1, pv1, absp1) <- 
                 checkPattern' flex ins (Domain av1 ki1 $ dec1 `compose` decEr) p1
              av2 <- whnf (update env1 y pv1) a2
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
              return (flex, ins, cxt', maybeErase $ VarP y, xv, False)

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
                 when (isFunType av) $ fail ("absurd pattern " ++ show p ++ " does not match function types, like " ++ show av)     
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
          ConP pi n ps | coPat pi == DefPat -> do
            PatSig ns pat v <- lookupSymb n
            unless (length ns == length ps) $ fail ("underapplied defined pattern in " ++ show p)
            let p = patSubst (zip ns ps) pat
            checkPattern' flex ins domEr p

          ConP pi n pl -> do
                 let co = coPat pi
{- TODO
                 enter ("can only match non parametric arguments") $
                   leqPolM (polarity dec) (pprod defaultPol)
-}
                 when (isFunType av) $ fail ("higher-order matching of pattern " ++ show p ++ " at type " ++ show av ++ " not allowed")     
-- TODO: ensure that matchings against erased arguments are forced 
--                 when (erased dec) $ throwErrorMsg $ "checkPattern: cannot match on erased argument " ++ show p ++ " : " ++ show av 
-- WRONG:                 let flex1 = if erased' then MaxMatches 1 av : flex else flex
                 let flex1 = flex

                 (ConSig nPars sz recOccs vc dataName _) <- lookupSymb n

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
                 vc <- instConLType n nPars vc isSz isFlex =<< force av
{-
                 vc <- case sz of
                         Nothing -> instConType n nPars vc =<< force av
                         Just vc -> instConType n (nPars+1) vc =<< force av
-}
                 -- check that size argument of coconstr is dotted
                 when (co == CoCons && isJust sz) $ do
                   let sizep = head pl  -- 2012-01-22: WAS (pl !! nPars)
                   unless (isDotPattern sizep) $
                     fail $ "in pattern " ++ show p  ++ ", coinductive size sub pattern " ++ show sizep ++ " must be dotted"

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
                 
                 (flex',ins',cxt',vc',ple,pvs,absp) <- checkPatterns decEr flex1 ins vc pl
                 when (isFunType vc') $ fail ("higher-order matching of pattern " ++ show p ++ " of type " ++ show vc' ++ " not allowed")  
                 let flexgen = concat $ map (\ g -> case g of
                        DotFlex i _ _ -> [i]
                        MaxMatches{} -> []) flex'
                     -- fst $ unzip flex'
--                  av1 <- sing (environ cxt') (patternToExpr p) vc' 
--                  av2 <- sing (environ cxt') (patternToExpr p) av 
--                  subst <- local (\ _ -> cxt') $ inst flexgen VSet av1 av2
                 
                 -- need to evaluate the erased pattern!
                 let pe = ConP pi n ple -- erased pattern
                 let pv0 = VDef (DefId (ConK co) n) `VApp` pvs 
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

                 return (flex', ins'', cxt' { context = delta'' }, 
                         maybeErase pe, pv, absp)
{- DO NOT UPDATE measure here, its done in checkRHS
                 return (flex', ins'', cxt' { context = delta'', environ = (environ cxt') { envBound = mmu' } }, vb',
                         maybeErase pe, absp)
-}


          DotP e -> do
{-
            -- create a unique identifier for the dot pattern
            l <- getLen
            let xp = spaceToUnderscore (show e) ++ "#" ++ show l 
-}
            -- create an informative, but irrelevant identifier for dot pattern
            let xp = fresh $ "." ++ case e of Var z -> suggestion z; _ -> Util.parens $ show e 
            newWithGen xp domEr $ \ k xv -> do
                       cxt' <- ask
                       -- traceCheck ("Returning type " ++ show vb) $
                       return (DotFlex k e domEr : flex
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

{- RETIRED
-- checkDot does not need to extract
checkDot :: Substitution -> DotFlex -> TypeCheck ()
checkDot subst (i,(e,it)) = enter ("dot pattern " ++ show e) $ 
  case (lookup i subst) of
    Nothing -> fail $ "not instantiated"
    Just v -> do 
      tv <- substitute subst (typ it)
      ask >>= \ ce -> traceCheckM ("checking dot pattern " ++ show ce ++ " |- " ++ show e ++ " : " ++ show (decor it) ++ " " ++ show tv) 
      applyDec (decor it) $ do
        checkExpr e tv
        v' <-  whnf' e -- TODO: has subst erased terms?
        enter ("inferred value " ++ show v ++ " does not match given dot pattern value " ++ show v') $
          eqVal Pos tv v v'  
-}

-- checkDot does not need to extract
checkGoal :: Substitution -> Goal -> TypeCheck ()
checkGoal subst (DotFlex i e it) = enter ("dot pattern " ++ show e) $ 
  case (lookup i subst) of
    Nothing -> recoverFail $ "not instantiated"
    Just v -> do 
      tv <- substitute subst (typ it)
      ask >>= \ ce -> traceCheckM ("checking dot pattern " ++ show ce ++ " |- " ++ show e ++ " : " ++ show (decor it) ++ " " ++ show tv) 
--      applyDec (decor it) $ do
      resurrect $ do -- consider a DotP e always as irrelevant!
        checkExpr e tv
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
         else -- n == 1 and 
           "more than one constructor matches type " ++ show av'
       else return ())
   mcenvs  


checkRHS :: Substitution -> Expr -> TVal -> TypeCheck (Kinded Extr)
checkRHS ins rhs v = do
   traceCheckM ("checking rhs " ++ show rhs ++ " : " ++ show v)  
   enter "right hand side" $ do
     -- first updated measure according to substitution for dot variables
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
      (DataSig { numPars = np, symbTyp = tv, positivity = posl}) <- lookupSymb d1
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

-- match v1 against v2 by unification , yielding a substition
inst :: Pol -> [Int] -> TVal -> Val -> Val -> TypeCheck Substitution
inst pos flex tv v1 v2 = ask >>= \ cxt -> enterDoc (text ("inst " ++ show (context cxt) ++ " |-") <+> prettyTCM v1 <+> text ("?<=" ++ show pos) <+> prettyTCM v2 <+> colon <+> prettyTCM tv) $ do 
--  case tv of
--    (VPi dec x av env b) -> 
  case (v1,v2) of
    (VGen k, VGen j) | k == j -> return []
    (VGen k,_) | elem k flex -> do
                       l <- getLen
                       noc <- nocc l v1 v2 
                       case noc of 
                         True -> return [(k,v2)]
                         False -> throwErrorMsg "occurs check failed"
    (_,VGen k) | elem k flex -> do
                   l <- getLen
                   noc <- nocc l v2 v1 
                   case noc of 
                         True -> return [(k,v1)]
                         False -> throwErrorMsg "occurs check failed"
{- MOVED to unifyIndices
    -- injectivity of data type constructors is unsound in general
    (VApp (VDef (DefId Dat d1)) vl1, VApp (VDef (DefId Dat d2)) vl2) | d1 == d2 ->  do
         sig <- gets signature 
         instList flex (symbTyp (lookupSig d1 sig)) vl1 vl2
-}

    (VApp (VDef (DefId DatK d1)) vl1, VApp (VDef (DefId DatK d2)) vl2) | d1 == d2 ->  do
         (DataSig { numPars = np, symbTyp = tv, positivity = posl }) <- lookupSymb d1 
         instList' np posl flex tv vl1 vl2 -- ignore parameters (first np args)
           -- this is sound because we have irrelevance for parameters
           -- we assume injectivity for indices


    (VApp (VDef (DefId (ConK _) c1)) vl1,VApp (VDef (DefId (ConK _) c2)) vl2) | c1 == c2 -> do
         sige <- lookupSymb c1
         instList [] flex (symbTyp sige) vl1 vl2
    (VSucc v1',VSucc v2') -> instWh pos flex tv v1' v2'
    (VSucc v, VInfty) -> instWh pos flex tv v VInfty
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
            return []

instList :: [Pol] -> [Int] -> TVal -> [Val] -> [Val] -> TypeCheck Substitution
instList posl flex tv vl1 vl2 = do
  -- vl1 <- mapM whnfClos vl1
  -- vl2 <- mapM whnfClos vl2
  instList' 0 posl flex tv vl1 vl2

-- unify lists, ignoring the first np items
instList' :: Int -> [Pol] -> [Int] -> TVal -> [Val] -> [Val] -> TypeCheck Substitution
instList' np posl flex tv [] [] = return []
instList' np posl flex tv (v1:vl1) (v2:vl2) = do
  v1 <- whnfClos v1
  v2 <- whnfClos v2
  if (np <= 0 || isMeta flex v1 || isMeta flex v2) then 
    case tv of
      (VQuant Pi x dom env b) -> do    
        subst <- inst (headPosl posl) flex (typ dom) v1 v2
        vl1' <- mapM (substitute subst) vl1
        vl2' <- mapM (substitute subst) vl2
        v    <- substitute subst v1
        env' <- substitute subst env
        vb   <- whnf (update env' x v) b
        subst' <- instList' (np - 1) (tailPosl posl) flex vb vl1' vl2'
        compSubst subst subst'
   else
    case tv of
      (VQuant Pi x dom env b) -> do    
        vb   <- whnf (update env x v2) b
        instList' (np - 1) (tailPosl posl) flex vb vl1 vl2
instList' np pos flex tv vl1 vl2 = fail $ "internal error: instList' " ++ show (np,pos,flex,tv,vl1,vl2) ++ " not handled" 

headPosl :: [Pol] -> Pol      
headPosl [] = mixed
headPosl (pos:_) = pos

tailPosl :: [Pol] -> [Pol]      
tailPosl [] = []
tailPosl (_:posl) = posl


isMeta :: [Int] -> Val -> Bool
isMeta flex (VGen k) = k `elem` flex
isMeta _ _ = False

-- overload `subst`
class Substitute a where
  substitute :: Substitution -> a -> TypeCheck a

instance Substitute (Measure Val) where
  substitute subst (Measure mu) = do
    mu' <- mapM (substitute subst) mu
    return $ Measure mu'

instance Substitute (Bound Val) where
  substitute subst (Bound ltle mu1 mu2) = do
    mu1' <- substitute subst mu1
    mu2' <- substitute subst mu2
    return $ Bound ltle mu1' mu2'

-- substitute generic variable in value
instance Substitute Val where
  substitute subst v =  
    case v of
      VGen k -> case lookup k subst of
                  Nothing ->  return $ v
                  Just v' ->  return $ v'
      VApp v1 vl -> do v1' <- substitute subst v1
                       vl' <- mapM (substitute subst) vl
                       foldM app v1' vl'  -- does not do anything for empty list vl'
      VSing v1 vt -> do v1' <- substitute subst v1
                        vt' <- substitute subst vt
                        return $ vSing v1' vt'  -- TODO: Check reevaluation necessary? 

      VSucc v1  -> succSize  <$> substitute subst v1
      VMax  vs  -> maxSize   <$> mapM (substitute subst) vs
      VPlus vs  -> plusSizes <$> mapM (substitute subst) vs

      VCase v1 env cl -> do v1' <- substitute subst v1
                            env' <- substitute subst env
                            return $ VCase v1' env' cl
      VMeasured mu bv -> do
          mu' <- substitute subst mu
          bv'   <- substitute subst bv
          return $ VMeasured mu' bv'
      VGuard beta bv -> do
          beta' <- substitute subst beta
          bv'   <- substitute subst bv
          return $ VGuard beta' bv'

      VBelow ltle v -> VBelow ltle <$> substitute subst v

      VQuant pisig x dom env b ->  
          do dom'  <- Traversable.mapM (substitute subst) dom
             env' <- substitute subst env
             return $ VQuant pisig x dom' env' b
      VPair v1 v2 -> VPair <$> substitute subst v1 <*> substitute subst v2
      VLam x env b -> do env' <- substitute subst env
                         return $ VLam x env' b
      VClos env e  -> do env' <- substitute subst env
                         return $ VClos env' e
      VUp v1 vt -> do v1' <- substitute subst v1
                      vt' <- substitute subst vt
                      up False v1' vt'
      VSort s   -> substitute subst s >>= return . VSort
      VZero     -> return $ v
      VInfty    -> return $ v
      VIrr      -> return $ v 
      VDef id   -> return $ vDef id  -- because empty list of apps will be rem.
--      VCon co n -> return $ v
      VMeta x env n -> do env' <- substitute subst env
                          return $ VMeta x env' n
      _ -> error $ "substitute: internal error: not defined for " ++ show v

instance Substitute (Sort Val) where
  substitute subst s =
    case s of
      Set v -> do v' <- substitute subst v
                  return $ Set v'
      CoSet v -> substitute subst v >>= return . CoSet
      SortC c -> return $ SortC c

instance Substitute Domain where
  substitute subst dom = Traversable.mapM (substitute subst) dom

instance Substitute SemCxt where
  substitute subst delta = do  
    cxt' <- mapMapM (Traversable.mapM (substitute subst)) (cxt delta)
    return $ delta { cxt = cxt' }
{-
instance Substitute a => Substitute (Maybe a) where
  substitute subst Nothing  = return $ Nothing
  substitute subst (Just a) = substitute subst a >>= return . Just 
-}

-- substitute in environment
instance Substitute Env where
  substitute subst (Environ rho mmeas) = do
    rho' <- mapM (\(x,v) -> substitute subst v >>= return . (x,)) rho
    mmeas' <- Traversable.mapM (substitute subst) mmeas
    return $ Environ rho' mmeas'
{-
substitute subst [] = return $ []
substitute subst ((x,v):env) = do  v'   <- substitute subst v
                                 env' <- substitute subst env
                                 return $  (x,v'):env'
-}


-- "merge" substitutions by first applying the second to the first, then 
-- appending them
compSubst :: Substitution -> Substitution -> TypeCheck Substitution
compSubst subst1 subst2 = do
    subst1' <- mapM (\ (x,v) -> substitute subst2 v >>= return . (x,)) subst1
    return $ subst1' ++ subst2               
{-
compSubst subst1 subst2 = do
    let (dom1,tg1) = unzip subst1
    tg1' <- mapM (substitute subst2) tg1
    let subst1' = zip dom1 tg1'
    return $ subst1' ++ subst2               
-}

-- Size checking
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

mkConLType :: Int -> Expr -> Expr
mkConLType npars t = 
  let (sizetb:tel, t0) = typeToTele t
  in case spineView t0 of
    (d@(Def (DefId DatK _)), args) -> 
      let (pars, sizeindex : inds) = splitAt npars args
          i     = fresh "s!ze"
          args' = pars ++ Var i : inds
          core  = foldl App d args'
          tbi   = TBind i $ sizeDomain irrelevantDec
          tbj   = sizetb { boundDom = belowDomain irrelevantDec Lt (Var i) } 
          tel'  = tbi : tbj : tel
      in teleToType tel' core
    _ -> error $ "conLType " ++ show npars ++ " (" ++ show t ++ "): illformed constructor type"



-- * check wether the data type is sized type


-- check data declaration type 
-- called from typeCheckDeclaration (DataDecl{})
-- parameters : number of params, type 
szType :: Co -> Int -> TVal -> TypeCheck ()
szType co p tv = doVParams p tv $ \ tv' -> do
    let polsz = if co==Ind then Pos else Neg
    case tv' of
      VQuant Pi x (Domain av ki dec) env b | isVSize av && not (erased dec) && polarity dec == polsz -> return ()
      _ -> throwErrorMsg $ "not a sized type, target " ++ show tv' ++ " must have non-erased domain " ++ show Size ++ " with polarity " ++ show polsz
                 
-- * constructors of sized type

-- check data constructors
-- called from typeCheckConstructor
szConstructor :: Name -> Co -> Int -> TVal -> TypeCheck ()
szConstructor n co p tv = enterDoc (text ("szConstructor " ++ show n ++ " :") <+> prettyTCM tv) $ do 
  doVParams p tv $ \ tv' -> 
    case tv' of
       VQuant Pi x dom env b | isVSize (typ dom) -> 
          newWithGen x dom $ \ k xv -> do
            bv <- whnf (update env x xv) b 
            szSizeVarUsage n co p k bv
       _ -> fail $ "not a valid sized constructor: expected size quantification"

szSizeVarUsage :: Name -> Co -> Int -> Int -> TVal -> TypeCheck ()
szSizeVarUsage n co p i tv = enterDoc (text "szSizeVarUsage of" <+> prettyTCM (VGen i) <+> text "in" <+> prettyTCM tv) $ 
    case tv of
       VQuant Pi x dom env b -> do 
          let av = typ dom
          szSizeVarDataArgs n p i av  -- recursive calls of for D..i..
          enterDoc (text "checking" <+> prettyTCM av <+> text (" to be " ++
              (if co == CoInd then "antitone" else "isotone") ++ " in variable")
              <+> prettyTCM (VGen i)) $
            szMono co i av                -- monotone in i
          new x dom $ \ xv -> do
            bv <- whnf (update env x xv) b
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
     VApp (VDef (DefId DatK m)) vl | n == m -> do 
        v0 <- whnfClos $ vl !! p
        case v0 of
          (VGen i') | i' == i  -> do 
              let rargs = take p vl ++ drop (p+1) vl
              k <- getLen
              mapM_ (\ v -> (nocc k (VGen i) v) >>= boolToErrorDoc (text "variable" <+> prettyTCM (VGen i) <+> text "may not occur in" <+> prettyTCM v)) rargs
          _ -> failDoc (text "wrong size index" <+> prettyTCM v0 <+> text "at recursive occurrence" <+> prettyTCM tv)

-- not necessary: check for monotonicity above
--     {- case D' pars sizeArg args -}
--     VApp (VDef m) vl | n /= m -> do 

     VApp v1 vl -> mapM_ (\ v -> whnfClos v >>= szSizeVarDataArgs n p i) (v1:vl)

     VQuant Pi x dom env b -> do 
       szSizeVarDataArgs n p i (typ dom)
       new x dom $ \ xv -> do
          bv <- whnf (update env x xv) b
          szSizeVarDataArgs n p i bv
              
     VLam x env b -> 
       addName x $ \ xv -> do
         bv <- whnf (update env x xv) b
         szSizeVarDataArgs n p i bv           
     _ -> return ()

{- REMOVED, 2009-11-28, replaced by monotonicity check
     VGen i' -> return $ i' /= i
     VSucc tv' -> szSizeVarDataArgs n p i tv'
 -}

-- doVParams number_of_params constructor_or_datatype_signature
-- skip over parameters of type signature of a constructor/data type
doVParams :: Int -> TVal -> (TVal -> TypeCheck a) -> TypeCheck a
doVParams 0 tv k = k tv
doVParams p (VQuant Pi x dom env b) k = 
  new x dom $ \ xv -> do 
    bv <- whnf (update env x xv) b
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
    fail $ show tv ++ " is not a type of a cofun" -- ++ if co==Ind then "fun" else "cofun"
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
         

-- | endsInSizedCo i tv = True  if  tv = Gamma -> D pars i indexes
endsInSizedCo :: Int -> TVal -> TypeCheck ()
endsInSizedCo i tv  = enterDoc (text "endsInSizedCo:" <+> prettyTCM tv) $ do
   let fail1 = failDoc $ text "endsInSizedCo: target" <+> prettyTCM tv <+> text "of corecursive function is neither a CoSet or codata of size" <+> prettyTCM (VGen i) <+> text "nor a tuple type" 
   case tv of
      VSort (CoSet (VGen i)) -> return ()  
      VMeasured mu bv -> endsInSizedCo i bv

      -- case forall j <= i. C j coinductive in i
      VGuard (Bound Le (Measure [VGen j]) (Measure [VGen i'])) bv | i == i' ->
        endsInSizedCo j bv
      VGuard (Bound Lt (Measure [VGen j]) (Measure [VSucc (VGen i')])) bv | i == i' ->
        endsInSizedCo j bv
      -- case forall j < i. C j:  already coinductive in i !!
      VGuard (Bound Lt (Measure [VGen j]) (Measure [VGen i'])) bv | i == i' ->
        return ()

      VQuant Pi x dom env b -> new x dom $ \ gen -> do
         let av = typ dom
         isInd <- szUsed Ind i av

         unless isInd $
           szAntitone i av `newErrorDoc` 
            (text "type" <+> prettyTCM av <+> text "not lower semi continuous in" <+> prettyTCM (VGen i))

         bv <- whnf (update env x gen) b
         endsInSizedCo i bv
      VSing _ tv -> endsInSizedCo i =<< whnfClos tv
      VApp (VDef (DefId DatK n)) vl -> do 
         sige <- lookupSymb n 
         case sige of
            DataSig { numPars = np, isSized = Sized, isCo = CoInd } 
              | length vl > np -> do
                 v <- whnfClos $ vl !! np
                 if isVGeni v then return () else fail1
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
            DataSig { symbTyp = dv, numPars = np, isSized = sz, 
                      constructors = cis, isTuple = True } -> do
              -- match target of constructor against tv to instantiate
              --  c : ... -> D ps  -- ps = snd (cPatFam ci)
              mrhoci <- Util.firstJustM $ map (\ ci -> fmap (,ci) <$> nonLinMatchList False emptyEnv (snd $ cPatFam ci) vl dv) cis
              case mrhoci of
                Nothing -> failDoc $ text "endsInSizedCo: panic: target type" <+> prettyTCM tv <+> text "is not an instance of any constructor"
                Just (rho,ci) -> enter ("endsInSizedCo: detected tuple target, checking components") $ 
                  fieldsEndInSizedCo i (cFields ci) rho 
            _ -> fail1
      _ -> fail1
{- failDoc $ text "endsInSizedCo: target" <+> prettyTCM tv <+> text "of corecursive function is neither a function type nor a codata nor a tuple type"
-}

fieldsEndInSizedCo :: Int -> [FieldInfo] -> Env -> TypeCheck ()
fieldsEndInSizedCo i fis rho0 = enter ("fieldsEndInSizedCo: checking fields of tuple type " ++ show fis ++ " in environment " ++ show rho0) $
  loop fis rho0 where  
    loop [] rho = return ()
    -- nothing to check for erased index fields
    loop (f : fs) rho | fClass f == Index && erased (fDec f) = 
      loop fs rho
    loop (f : fs) rho | fClass f == Index = do
      tv <- whnf rho (fType f) 
      endsInSizedCo i tv
      loop fs rho
    loop (f : fs) rho = do
      tv <- whnf rho (fType f) 
      when (not $ erased (fDec f)) $ endsInSizedCo i tv
      -- for non-index fields, value is not given by matching, so introduce
      -- generic value
      new (fName f) (Domain tv defaultKind (fDec f)) $ \ xv -> do
        let rho' = update rho (fName f) xv
        -- do not need to check erased fields?
        loop fs rho'



{-
-- | endsInSizedCo v tv = True  if  tv = Gamma -> D pars v' indexes
-- for v = v' : Size
endsInSizedCo :: Val -> TVal -> TypeCheck Bool
endsInSizedCo v tv  = -- traceCheck ("endsInCo: " ++ show tv) $
   case tv of
      VPi dec x av env b -> new x (Domain av dec) $ \ gen -> do
         bv <- whnf (update env x gen) b
         endsInSizedCo v bv
      VApp (VDef (DefId Dat n)) vl -> do 
         sig <- gets signature
         case (lookupSig n sig) of
            DataSig { numPars = np, isSized = Sized, isCo = CoInd } 
              | length vl > np ->
                 eqValBool vSize (vl !! np) v
            _ -> return False
      _ -> return False
-}

endsInCo :: TVal -> TypeCheck Bool
endsInCo tv  = -- traceCheck ("endsInCo: " ++ show tv) $
   case tv of
      VQuant Pi x dom env b -> new x dom $ \ gen -> do
         bv <- whnf (update env x gen) b
         endsInCo  bv

{-
      -- if not applied, it cannot be a sized type
      VDef (DefId Dat n) -> do 
         sig <- gets signature
         case (lookupSig n sig) of
            DataSig { isCo = CoInd } -> -- traceCheck ("found non-sized coinductive target") $ 
               return True
            _ -> return False
-}

      VApp (VDef (DefId DatK n)) vl -> do 
         sige <- lookupSymb n
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
           _ -> fail "admPattern: IMPOSSIBLE: non-projection pattern for record type"
      VQuant Pi x dom env b -> newWithGen x dom $ \ k xv -> do
         bv <- whnf (update env x xv) b
  {-
         if p is successor pattern
         check that bv is admissible in k, returning subset of [Ind, CoInd]       
         p is usable if either CoInd or it is a var or dot pattern and Ind  
-}
         if isSuccessorPattern p then do
           inco <- admType k bv
           when (CoInd `elem` inco && not (shallowSuccP p)) $
             fail ("cannot match coinductive size against deep successor pattern " 
                    ++ show p ++ "; type: " ++ show tv)
           if (CoInd `elem` inco)
              || (inco /= [] && completeP p) 
            then return (p, inco)
            else return (UnusableP p, inco)
          else return (p, [])

      _ -> fail "admPattern: IMPOSSIBLE: pattern for a non-function type"

admType :: Int -> TVal -> TypeCheck [Co]
admType i tv = enter ("admType: checking " ++ show tv ++ " admissible in v" ++ show i) $
    case tv of 
       VQuant Pi x dom@(Domain av _ _) env b -> do
          isInd <- szUsed Ind i av
          when (not isInd) $
            szAntitone i av `newErrorMsg` 
             ("type " ++ show av ++ " not lower semi continuous in v" ++ show i)  
          new x dom $ \ gen -> do
            bv <- whnf (update env x gen) b
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
         (VApp (VDef (DefId DatK n)) vl) -> 
             do sige <- lookupSymb n
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
       VQuant Pi x dom env b -> new x dom $ \ (VGen k) -> do
         bv <- whnf' b
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
       VQuant Pi x dom env b -> do
            szLowerSemiCont i (typ dom)
            new' x dom $ do
              bv <- whnf' b
              szCheckIndFunSize i bv
       _ -> szMonotone i tv

{- szLowerSemiCont

 - check for lower semi-continuity [Abel, CSL 2006]
 - current approximation: inductive type or antitone
 -}
szLowerSemiCont :: Int -> TVal -> TypeCheck ()
szLowerSemiCont i av = -- traceCheck ("szlowerSemiCont: checking " ++ show av ++ " lower semi continuous in v" ++ show i) $
   ((szAntitone i av) `catchError` 
      (\ msg -> -- traceCheck (show msg) $
                   szInductive i av)) 
        `newErrorMsg` ("type " ++ show av ++ " not lower semi continuous in v" ++ show i)  
                                                     

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
      VQuant Pi x dom env b -> do
         l <- getLen
         let jobs' = (addJob l (typ dom) jobs)
         new' x dom $ do
           bv <- whnf' b
           admEndsInCo bv firstVar jobs' 

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
         sige <- lookupSymb n
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
              
{-
szMono :: Co -> Int -> TVal -> TypeCheck Bool
szMono co i tv = 
  (do case co of
         Ind   -> szMonotone i tv
         CoInd -> szAntitone i tv
      return True
  ) `catchError` (\ _ -> return False)
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
   tv' <- substitute [(i,si)] tv
   leqVal Pos vTopSort tv tv'

szAntitone :: Int -> TVal -> TypeCheck ()
szAntitone i tv = traceCheck ("szAntitone: " -- ++ show delta ++ " |- " 
                              ++ show tv ++ " anti(v" ++ show i ++ ")?") $
 do
   let si = VSucc (VGen i)
   tv' <- substitute [(i,si)] tv
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
             do sige <- lookupSymb n
                case sige of
                  DataSig { numPars = p, isSized = Sized, isCo =  co' } | co == co' && length vl > p -> 
                      -- p is the number of parameters
                      -- it is also the index of the size argument
                      do s <- whnfClos $ vl !! p
                         case s of
                           VGen i' | i == i' -> return ()
                           _ -> fail $ "expected size variable" 
                  _ -> fail $ "expected (co)inductive sized type"
         _ -> fail $ "expected (co)inductive sized type"

