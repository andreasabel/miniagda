module Termination where

import Abstract
import Data.List as List
import qualified Data.Set as Set
import Debug.Trace

import System


data Order = Lt
           | Le
           | Un
           | Mat (Matrix Order) -- square matrices only
           deriving (Show,Eq,Ord)

ordRing :: SemiRing Order
ordRing = SemiRing { add = maxO , mul = comp , one = Le , zero = Un }

comp :: Order -> Order -> Order
comp Lt Un = Un
comp Lt (Mat m) = comp Lt (collapse m)
comp Lt _ = Lt
comp Le o = o
comp Un _ = Un
comp (Mat m1) (Mat m2) = if (ok m1 m2) then
                             Mat $ mmul ordRing m1 m2
                         else
                             comp (collapse m1) (collapse m2)
comp (Mat m) Le = Mat m
comp (Mat m) Un = Un
comp (Mat m) Lt = comp (collapse m) Lt

maxO :: Order -> Order -> Order
maxO o1 o2 = case (o1,o2) of 
               (_,Lt) -> Lt
               (Lt,_) -> Lt
               (Un,_) -> o2
               (_,Un) -> o1
               (Le,Le) -> Le
               (Mat m1, Mat m2) -> if (ok m1 m2) then
                                       Mat $ madd ordRing m1 m2
                                   else
                                       maxO (collapse m1) (collapse m2)
               (Mat m1,_) -> maxO (collapse m1) o2
               (_,Mat m2) -> maxO o1 (collapse m2)

minO :: Order -> Order -> Order
minO o1 o2 = case (o1,o2) of
               (Un,_) -> Un
               (_,Un) -> Un
               (Lt,_) -> o2
               (_,Lt) -> o1
               (Le,Le) -> Le
               (Mat m1, Mat m2) -> if (ok m1 m2) then
                                       Mat $ minM m1 m2
                                   else
                                       minO (collapse m1) (collapse m2)
               (Mat m1,_) -> minO (collapse m1) o2
               (_,Mat m2) -> minO o1 (collapse m2)

minM :: Matrix Order -> Matrix Order -> Matrix Order
minM m1 m2 = [ minV x y | (x,y) <- zip m1 m2]

minV :: Vector Order -> Vector Order -> Vector Order
minV v1 v2 = [ minO x y | (x,y) <- zip v1 v2]

supremum :: [Order] -> Order
supremum = foldr maxO Un

infimum :: [Order] -> Order
infimum = foldr minO Lt

collapse :: Matrix Order -> Order
collapse m = let d = diag m
             in 
               infimum d

type Vector a = [a]
type Matrix a = [Vector a]


--- 
-- matrix stuff

data SemiRing a = SemiRing { add :: (a -> a -> a) , mul :: (a -> a -> a) , one :: a , zero :: a } 

ssum :: SemiRing a -> Vector a -> a
ssum sem v = foldl (add sem) (zero sem) v 

vadd :: SemiRing a -> Vector a -> Vector a -> Vector a
vadd sem v1 v2 = [ (add sem) x y | (x,y) <- zip v1 v2]

scalarProdukt :: SemiRing a -> Vector a -> Vector a -> a
scalarProdukt sem xs ys = ssum sem [(mul sem) x y  | (x,y) <- zip xs ys]

madd :: SemiRing a -> Matrix a -> Matrix a -> Matrix a
madd sem m1 m2 = [ vadd sem x y | (x,y) <- zip m1 m2]

transp :: Matrix a -> Matrix a 
trans [] = []
transp y = [[ z!!j | z<-y] | j<-[0..s]]
    where
    s = length (head y)-1

mmul :: SemiRing a -> Matrix a -> Matrix a -> Matrix a
mmul sem m1 m2 = [[scalarProdukt sem r c | c <- transp m2] | r<-m1 ]

diag :: Matrix a -> Vector a
diag [] = []
diag m = [ (m !! j) !! j | j <- [ 0..s] ] 
   where
     s = length (head m) - 1

elems :: Matrix a -> Vector a
elems m = concat m

ok :: Matrix a -> Matrix a -> Bool
ok m1 m2 = (length m1) == length m2 

---

compareArgs :: [Pattern] -> [Expr] -> Int -> Matrix Order
compareArgs _ [] _ = [[]]
compareArgs [] _ _ = [[]]
compareArgs pl el ar_g = 
        let 
            diff = ar_g - length el 
            fill = if diff > 0 then
                       replicate diff (replicate (length pl) Un)
                   else []
            cmp = map (\ e -> (map (compareExpr e) pl)) el
        in
          cmp ++ fill
            
             
compareExpr :: Expr -> Pattern -> Order
compareExpr e p = 
   case (e,p) of 
      (_,DotP e') -> case exprToPattern e' of
                       Nothing -> Un
                       Just p' -> compareExpr e p'
      (Var i,p) -> compareVar i p 
      (App (Var i) _,p) -> compareVar i p 
      (App (Con _ n) args,(ConP _ n2 pl))| n == n2 -> 
               if (length pl == length args && length args >= 2) 
               then 
                   Mat (map (\ e -> (map (compareExpr e) pl)) args)
               else
                   infimum $ zipWith compareExpr args pl
      (Con _ n,ConP _ n2 []) | n == n2 -> Le     
      (_,ConP Ind n pl) -> comp Lt $ infimum $ map (compareExpr e) pl 
      (Succ e2,SuccP p2) -> compareExpr e2 p2     
      _ -> Un

compareVar :: Name -> Pattern -> Order
compareVar n p = 
    case p of
      (VarP n2) -> if n == n2 then Le else Un
      (ConP Ind c pl) -> comp Lt (supremum (map (compareVar n) pl))
      (SuccP p2) -> comp Lt (compareVar n p2)
      (DotP e) -> case (exprToPattern e) of
                    Nothing -> Un
                    Just p' -> compareVar n p'
      (ConP CoInd _ _ ) -> Un -- not well-founded
      

exprToPattern :: Expr -> Maybe Pattern
exprToPattern e = 
    case e of
      Var n -> Just $ VarP n
      (Succ e) -> case exprToPattern e of
                    Nothing -> Nothing
                    Just p -> Just $ SuccP p
      (App (Con co n) el) -> case exprsToPatterns el of
                               Nothing -> Nothing
                               Just pl -> Just $ ConP co n pl
      (Con co n) -> Just $ ConP co n []
      _ -> Nothing

exprsToPatterns :: [Expr] -> Maybe [Pattern]
exprsToPatterns [] = Just []
exprsToPatterns (e:el) = case exprToPattern e of
                           Nothing -> Nothing
                           Just p -> case exprsToPatterns el of
                                       Nothing -> Nothing
                                       Just pl -> Just (p:pl)

-------------------

terminationCheck :: [(TypeSig,Co,[Clause])] -> IO ()
terminationCheck funs =
       let tl = terminationCheckFuns funs
           nl = map fst tl
           bl = map snd tl
           nl2 = [ n | (n,b) <- tl , b == False ]
       in case (and bl) of
            True -> case nl of
                      [f] -> return ()
                      _ -> return ()
            False -> case nl of
                    [f] -> putStrLn ("Termination check for function " ++ f ++ " fails ") 
                    _   -> putStrLn ("Termination check for mutual block " ++ show nl ++ " fails for " ++ show nl2) 
                                   

terminationCheckFuns :: [ (TypeSig,Co , [Clause]) ] -> [(Name,Bool)]
terminationCheckFuns funs =
    let beh = recBehaviours funs 
    in
      zip (map fst beh) (map (checkAll . snd ) beh )
---

type Index = Name

data Call = Call { source :: Index , target :: Index , matrix :: CallMatrix }  
            deriving (Eq,Show,Ord)

type CallMatrix = Matrix Order



callComb :: Call -> Call -> Call
callComb (Call s1 t1 m1) (Call s2 t2 m2) = Call s2 t1 (mmul ordRing m1 m2)

cgComb :: [Call] -> [Call] -> [Call]
cgComb cg1 cg2 = [ callComb c1 c2 | c1 <- cg1 , c2 <- cg2 , (source c1 == target c2)]

complete :: [Call] -> [Call]
complete cg =
              let cg' = Set.fromList cg
                  cg'' = Set.union cg' (Set.fromList $ cgComb cg cg ) in
              if (cg'' == cg') then Set.toList cg'' else complete (Set.toList cg'')

checkAll :: [Call] -> Bool
checkAll x = all checkIdem x

checkIdem :: Call -> Bool
checkIdem cg = let cgcg = callComb cg cg
                   d = diag (matrix cgcg)
                   containsDecr = any isDecr d
               in (not (cg == cgcg)) || containsDecr

isDecr :: Order -> Bool
isDecr o = case o of
             Lt -> True
             (Mat m) -> any isDecr (diag m)
             _ -> False
             
recBehaviours :: [ (TypeSig, Co , [Clause] ) ] -> [(Name,[Call])]
recBehaviours funs = let names = map fst $ collectNames funs
                         cg = ccFunDecl funs
                     in groupCalls names [ c | c <- cg , (target c == source c) ]

groupCalls :: [Name] -> [Call] -> [(Name,[Call])]
groupCalls [] _ = []
groupCalls (n:nl) cl = (n, [ c | c <- cl , (source c == n) ]) : groupCalls nl cl

ccFunDecl :: [ ( TypeSig, Co , [Clause]) ] -> [Call]
ccFunDecl funs = complete $ collectCGFunDecl funs

collectCGFunDecl :: [(TypeSig, Co , [Clause])] -> [Call]
collectCGFunDecl funs =
    let names = collectNames funs
    in
      concatMap (collectClauses names) funs
          where
            collectClauses names ((TypeSig n _),_,cll) = collectClause names n cll
            collectClause names n ((Clause pl rhs):rest) = 
                (collectCallsExpr names n pl rhs) ++ (collectClause names n rest) 
            collectClause names n [] = []

arity :: [Clause] -> Int
arity [] = 0
arity (Clause pl e:l) = length pl

collectNames [] = []                
collectNames ((TypeSig n _,_,cl):rest) = (n,arity cl) : (collectNames rest)


collectCallsExpr :: [(Name,Int)] -> Name -> [Pattern] -> Expr -> [Call]
collectCallsExpr nl f pl e =
    case e of
      (App (Def g) args) -> let calls = concatMap (collectCallsExpr nl f pl) args
                                gIn = lookup g nl 
                            in
                              case gIn of
                                Nothing -> calls
                                Just ar_g -> let (Just ar_f) = lookup f nl
                                                 (Just f') = List.elemIndex (f,ar_f) nl
                                                 (Just g') = List.elemIndex (g,ar_g) nl
                                                 m = compareArgs pl args ar_g
                                                 cg = Call { source = f
                                                           , target = g
                                                           , matrix = m }
                                             in cg:calls
      (Def g) ->  collectCallsExpr nl f pl (App (Def g) []) 
      (App e args) -> concatMap (collectCallsExpr nl f pl) (e:args)
      (Lam _ e1) -> collectCallsExpr nl f pl e1
      (Pi _ e1 e2) -> (collectCallsExpr nl f pl e1) ++
                              (collectCallsExpr nl f pl e2)
      (Succ e1) -> collectCallsExpr nl f pl e1
      _ -> []

