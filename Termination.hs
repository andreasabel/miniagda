module Termination where

import Abstract
import Data.List as List
import qualified Data.Set as Set
import Debug.Trace

import System


data Order = Lt
           | Le
           | Un
           deriving (Show,Eq,Ord)

ordRing :: SemiRing Order
ordRing = SemiRing { add = maxO , mul = comp , one = Le , zero = Un }

comp :: Order -> Order -> Order
comp Lt Un = Un
comp Lt _ = Lt
comp Le o = o
comp Un _ = Un

maxO :: Order -> Order -> Order
maxO o1 o2 = case (o1,o2) of 
               (_,Lt) -> Lt
               (Lt,_) -> Lt
               (Un,_) -> o2
               (_,Un) -> o1
               (Le,Le) -> Le

minO :: Order -> Order -> Order
minO o1 o2 = case (o1,o2) of
               (Un,_) -> Un
               (_,Un) -> Un
               (Lt,_) -> o2
               (_,Lt) -> o1
               (Le,Le) -> Le

supremum :: [Order] -> Order
supremum = foldr maxO Un

infimum :: [Order] -> Order
infimum = foldr minO Lt

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
madd sem v1 v2 = [ vadd sem x y | (x,y) <- zip v1 v2]

transp :: Matrix a -> Matrix a 
transp y = [[ z!!j | z<-y] | j<-[0..s]]
    where
    s = length (head y)-1

mmul :: SemiRing a -> Matrix a -> Matrix a -> Matrix a
mmul sem m1 m2 = [[scalarProdukt sem r c | c <- transp m2] | r<-m1 ]

diag :: Matrix a -> Vector a
diag m = [ (m !! j) !! j | j <- [ 0..s] ] 
   where
     s = length (head m) - 1

elems :: Matrix a -> Vector a
elems m = concat m




---


compareArgs :: [Pattern] -> [Expr] -> [Int] -> [Int] -> Matrix Order
compareArgs [] [] _ _ = [[]]
compareArgs pl el admf admg = let pl' = zip pl [0..]
                                  el' = zip el [0..]
                              in
                                map (\ e -> (map (compareExpr' e admf admg) pl')) el'   
            
             
compareExpr' :: (Expr,Int) -> [Int] -> [Int] -> (Pattern,Int) -> Order
compareExpr' (e,i) admf admg (p,j) =  
    let bj = elem j admf
        bi = elem i admg
    in
      case (bj && bi) of
        True -> compareSize e p -- both are admissble size arguments
        False -> compareExpr e p 

compareExpr :: Expr -> Pattern -> Order
compareExpr e p = 
   case (e,p) of 
      (_,DotP e') -> case exprToPattern e' of
                       Nothing -> Un
                       Just p' -> compareExpr e p'
      (Var i,p) -> compareVar i p 
      (App (Con _ n) args,(ConP _ n2 pl))| n == n2 -> infimum $ zipWith compareExpr args pl
      (Con _ n,ConP _ n2 []) | n == n2 -> Le     
      (Succ e2,SuccP p2) -> compareExpr e2 p2     
      (App (Var f) args,VarP g) | f == g -> Le

      _ -> Un

compareVar :: Name -> Pattern -> Order
compareVar n p = 
    case p of
      (VarP n2) -> if n == n2 then Le else Un
      (ConP Ind c pl) -> comp Lt (supremum (map (compareVar n) pl))
      (SuccP p2) -> Un -- not well-founded
      (DotP e) -> case (exprToPattern e) of
                    Nothing -> Un
                    Just p' -> compareVar n p'
      (ConP CoInd _ _ ) -> Un -- not well-founded
      _ -> error $ "comparevar " ++ show n ++ "\n" ++ show p

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


compareSize :: Expr -> Pattern -> Order
compareSize e p = -- trace ("compareSize " ++ show (e,p)) $
    case (e,p) of 
      (_,DotP e') -> case exprToPattern e' of
                       Nothing -> Un
                       Just p' -> compareSize e p'
      (Var i,p) -> compareSizeVar i p 
      (Succ e2,SuccP p2) -> compareSize e2 p2           
      _ -> Un
                                        
compareSizeVar :: Name -> Pattern -> Order
compareSizeVar n p = 
    case p of
      (VarP n2) -> if n == n2 then Le else Un
      (SuccP p2) -> comp Lt (compareSizeVar n p2)
      (DotP e) -> case (exprToPattern e) of
                    Nothing -> Un
                    Just p' -> compareSizeVar n p'
      _ -> Un


-------------------

terminationCheckDecl :: Declaration -> [[Int]] -> Bool
terminationCheckDecl (FunDecl co funs) adml =
       let tl = terminationCheckFuns funs adml
           nl = map fst tl
           bl = map snd tl
           nl2 = [ n | (n,b) <- tl , b == False ]
       in case (and bl) of
            True -> case nl of
                      [f] -> True
                      _ -> True
            False -> case nl of
                    [f] -> trace ("Termination check for function " ++ f ++ " fails ") $
                                    False
                    _   -> trace ("Termination check for mutual block " ++ show nl ++ " fails for " ++ show nl2) $
                                    False



terminationCheckFuns :: [ (TypeSig,[Clause]) ] -> [[Int]] -> [(Name,Bool)]
terminationCheckFuns funs adml =
    let beh = recBehaviours funs adml
    in
      zip (map fst beh) (map (checkAll . snd ) beh )
---

type Edge = (Int,Int,Order)

type Path = [Name]

data CallGraph = CG { source :: Name , target :: Name , edges :: Set.Set Edge , path :: Path }
               deriving (Show)

instance Eq CallGraph where
    (CG s t e _) == (CG s2 t2 e2 _) = (s,t,e) == (s2,t2,e2)  

instance Ord CallGraph where
    (CG s t e _) <= (CG s2 t2 e2 _) = (s,t,e) <= (s2,t2,e2) 

composeCG :: CallGraph -> CallGraph -> CallGraph
composeCG cg1 cg2 =
        let l3 = [ ( s1 , t2 , comp o1 o2) | 
                   (s1,t1,o1) <- Set.toList $ edges cg1
                 , (s2,t2,o2) <- Set.toList $ edges cg2
                 , t1 == s2 ]
        in  CG { source = (source cg1)
               , target = (target cg2)
               , edges = Set.fromList l3
               , path = (path cg1) ++ (path cg2)}

cgComb :: Set.Set CallGraph -> Set.Set CallGraph -> Set.Set CallGraph
cgComb cg1 cg2 = Set.fromList ( [ composeCG c1 c2 | 
                                  c1 <- (Set.toList cg1)
                                , c2 <- (Set.toList cg2)
                                , (target c1 == source c2)])

complete :: Set.Set CallGraph -> Set.Set CallGraph
complete cg = let cg' = Set.union cg (cgComb cg cg) 
              in
                if (cg' == cg) then cg' else complete cg'


checkAll :: [CallGraph] -> Bool
checkAll x = all checkIdem x

checkIdem :: CallGraph -> Bool
checkIdem cg = let cgcg = composeCG cg cg
                   el = Set.toList $ edges cg
                   containsDecr = any isDecr el
               in (not (cg == cgcg)) || containsDecr

isDecr :: Edge -> Bool
isDecr (k1,k2,o) = k1 == k2 && case o of
                                  Lt -> True
                                  _ -> False
             
recBehaviours :: [ (TypeSig, [Clause] ) ] -> [[Int]] -> [(Name,[CallGraph])]
recBehaviours funs adml = let names = collectNames (map fst funs)
                              cg = ccFunDecl funs adml
                          in groupCalls names [ c | c <- cg , (target c == source c) ]

groupCalls :: [Name] -> [CallGraph] -> [(Name,[CallGraph])]
groupCalls [] _ = []
groupCalls (n:nl) cl = (n, [ c | c <- cl , (source c == n) ]) : groupCalls nl cl

ccFunDecl :: [ ( TypeSig,[Clause]) ] -> [[Int]] -> [CallGraph]
ccFunDecl funs adml = Set.toList $ complete $ Set.fromList $ collectCGFunDecl funs adml

collectCGFunDecl :: [(TypeSig,[Clause])] -> [[Int]] -> [CallGraph]
collectCGFunDecl funs adml =
    let names = collectNames (map fst funs)
    in
      concatMap (collectClauses names) funs
          where
            collectClauses names ((TypeSig n _),cll) = collectClause names n cll
            collectClause names n ((Clause pl rhs):rest) = 
                (collectCallsExpr names n adml pl rhs) ++ (collectClause names n rest) 
            collectClause names n [] = []

collectNames [] = []                                     
collectNames ((TypeSig n e):rest) = n : collectNames rest

collectCallsExpr :: [Name] -> Name -> [[Int]] -> [Pattern] -> Expr -> [CallGraph]
collectCallsExpr nl f adml pl e =
    case e of
      (App (Def g) args) -> let calls = concatMap (collectCallsExpr nl f adml pl) args
                                gIn = elem g nl 
                            in
                              case gIn of
                                False -> calls
                                True -> let (Just f') = List.elemIndex f nl
                                            (Just g') = List.elemIndex g nl
                                            admf = adml !! f'
                                            admg = adml !! g'
                                            m = compareArgs pl args admf admg
                                            el = Set.fromList $ matrixToEdges m 
                                            cg = CG { source = f
                                                    , target =  g
                                                    , edges = el
                                                    , path = ["<" ++ f ++ g ++ ">" ]}
                                        in cg:calls
      (Def g) ->  collectCallsExpr nl f adml pl (App (Def g) []) 
      (App e args) -> concatMap (collectCallsExpr nl f adml pl) (e:args)
      (Lam _ e1) -> collectCallsExpr nl f adml pl e1
      (Pi _ e1 e2) -> (collectCallsExpr nl f adml pl e1) ++
                              (collectCallsExpr nl f adml pl e2)
      (Succ e1) -> collectCallsExpr nl f adml pl e1
      _ -> []

matrixToEdges :: [[Order]] -> [Edge]
matrixToEdges m = concat $ mte 0 m
    where
      mte _ [] = []
      mte k (r:rs) = rowToEdges k r : (mte (k + 1) rs) where
          rowToEdges :: Int -> [Order] -> [Edge]
          rowToEdges k1 r = te 0 r
              where
                te _ [] = []
                te k2 (o:os) = let ys = te (k2 + 1) os 
                               in case o of
                                    Un -> ys
                                    _ -> (k1,k2,o) : ys