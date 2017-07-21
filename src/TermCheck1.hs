-- | Foetus termination checking

module TermCheck1 (terminationCheckDecl) where

import Abstract
import Termination


import Data.List as List
import qualified Data.Set as Set
import Debug.Trace
import System

terminationCheckDecl :: Declaration -> IO Bool
terminationCheckDecl (FunDecl co funs) =
    do let tl = terminationCheckFuns funs
           nl = map fst tl
           bl = map snd tl
           nl2 = [ n | (n,b) <- tl , b == False ]
       case (and bl) of
        True -> case nl of
                  [f] -> do putStrLn ("Termination check for " ++ f ++ " ok")
                            return True
                  _ -> do putStrLn ("Termination check for " ++ show nl ++ " ok")
                          return True
        False -> case nl of
                    [f] -> do putStrLn ("Termination check for function " ++ f ++ " fails ")
                              return False
                    _   -> do putStrLn ("Termination check for mutual block " ++ show nl ++ " fails for " ++ show nl2)
                              return False
terminationCheckDecl _ = return True

terminationCheckFuns :: [ (TypeSig,[Clause]) ] -> [(Name,Bool)]
terminationCheckFuns funs =
    let beh = recBehaviours funs
    in
      zip (map fst beh) (map (checkCalls . snd ) beh )
    where
      checkCalls = (hasLexOrd . toRecBehaviours )

type Index = Name

data Call = Call { source :: Index , target :: Index , matrix :: CallMatrix }
            deriving (Eq,Show,Ord)

type CallMatrix = Matrix Order

type CallGraph = Set.Set Call

union :: CallGraph -> CallGraph -> CallGraph
union = Set.union

callComb :: Call -> Call -> Call
callComb (Call s1 t1 m1) (Call s2 t2 m2) = Call s2 t1 (mmul ordRing m1 m2)

cgComb :: CallGraph -> CallGraph -> CallGraph
cgComb cg1 cg2 = Set.fromList ( [ callComb c1 c2 | c1 <- (Set.toList cg1) , c2 <- (Set.toList cg2) , (source c1 == target c2)])

complete :: CallGraph -> CallGraph
complete cg = let cg' = Set.union cg (cgComb cg cg) in
              if (cg' == cg) then cg' else complete cg'



toRecBehaviours :: [Call] -> RecBehaviours
toRecBehaviours m = map (diag . matrix ) m

recBehaviours :: [ (TypeSig, [Clause] ) ] -> [(Name,[Call])]
recBehaviours funs = let names = collectNames (map fst funs)
                         cg = ccFunDecl funs
                     in groupCalls names [ c | c <- Set.toList cg , (target c == source c) ]


groupCalls :: [Name] -> [Call] -> [(Name,[Call])]
groupCalls [] _ = []
groupCalls (n:nl) cl = (n,[ c | c <- cl , (source c == n) ]) : groupCalls nl cl

ccFunDecl :: [ ( TypeSig,[Clause]) ] -> CallGraph
ccFunDecl = complete . Set.fromList . collectCallsFunDecl

collectCallsFunDecl :: [(TypeSig,[Clause])] -> [Call]
collectCallsFunDecl funs =
    let names = collectNames (map fst funs)
    in
      concatMap (collectClauses names) funs
          where
            collectClauses names ((TypeSig n _),cll) = collectClause names n cll
            collectClause names n ((Clause (LHS pl) (RHS e)):rest) = (collectCallsExpr names n pl e) ++
                                                                    (collectClause names n rest)
            collectClause names n (absurd:rest) = collectClause names n rest
            collectClause names n [] = []

collectNames [] = []
collectNames ((TypeSig n e):rest) = n : collectNames rest


-- pre : all clauses have same pattern length
collectCallsExpr :: [Name] -> Name -> [Pattern] -> Expr -> [Call]
collectCallsExpr nl f pl e =
    case e of
      (App (Def g) args) -> let calls = concatMap (collectCallsExpr nl f pl) args
                                gIn = elem g nl
                            in
                              case gIn of
                                False -> calls
                                True -> let m = compareArgs pl args in
                                        (Call {source = f , target = g , matrix = m}):calls
      (Def g) ->  collectCallsExpr nl f pl (App (Def g) [])
      (App e args) -> concatMap (collectCallsExpr nl f pl) (e:args)
      (Lam _ e1) -> collectCallsExpr nl f pl e1
      (Pi (TBind _ e1) e2) -> (collectCallsExpr nl f pl e1) ++
                              (collectCallsExpr nl f pl e2)
      (Fun e1 e2) ->  (collectCallsExpr nl f pl e1) ++
                      (collectCallsExpr nl f pl e2)
      (Succ e1) -> collectCallsExpr nl f pl e1
      _ -> []


----------------
--lexicrophic ordering

type LexOrder arg = [arg]

type RecBehaviours = Matrix Order

okColumn :: [Order] -> Bool
okColumn v = any (== Lt) v && not (any (== Un) v)

okColumns :: RecBehaviours -> [Int]
okColumns m = let columns = transpose m
                  l = length columns - 1
                  in [ i | i <- [0..l] , okColumn (columns !! i ) ]

removeColumn :: RecBehaviours -> Int -> RecBehaviours
removeColumn m i = map (removeNth i) m


removeRows :: RecBehaviours -> Int -> RecBehaviours
removeRows [] i = []
removeRows (l:ls) i = if ((l !! i) == Lt) then removeRows ls i else (l:removeRows ls i)

removeNth :: Int -> [a] -> [a]
removeNth i l = (take i l) ++ (drop (i+1) l)

lexOrd :: RecBehaviours -> Maybe [Int]
lexOrd [] = Just []
lexOrd m = let ok = okColumns m
               in
                 case ok of
                   [] -> Nothing
                   (i:_) -> let m2 = removeColumn (removeRows m i) i
                                r = lexOrd m2
                            in case r of
                                 Nothing -> Nothing
                                 Just rord -> Just (i : (map (renumber i) rord))
    where
      renumber i x = if (x >= i) then (x+1) else x

hasLexOrd :: RecBehaviours -> Bool
hasLexOrd m = case (lexOrd m) of
                Nothing -> False
                Just _ -> True
