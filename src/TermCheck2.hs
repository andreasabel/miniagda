-- | Size change termination.

module TermCheck2 (terminationCheckDecl) where

import Abstract
import Termination

import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace

terminationCheckDecl :: Declaration -> [[Int]] -> Bool
terminationCheckDecl (FunDecl co funs) adml =
    trace ("adm " ++ show adml) $
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
                                  (Mat m) -> isDecrO (Mat m)
                                  _ -> False

-- a matrix is decreasing if one diagonal element is decreasing
isDecrO :: Order -> Bool
isDecrO Lt = True
isDecrO (Mat m) = any isDecrO (diag m)
isDecrO _ = False

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

-- pre : all clauses have same pattern length
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
                                    (Mat m) -> if (any (/= Un) (diag m)) then (k1,k2,o) : ys
                                               else ys

                                    _ -> (k1,k2,o) : ys
