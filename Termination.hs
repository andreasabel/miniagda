module Termination (terminationCheckDecl) where

import Abstract
import Data.List as List
import qualified Data.Set as Set
import Debug.Trace


terminationCheckDecl :: Declaration -> Bool
terminationCheckDecl (FunDecl co funs) = 
    let tl = terminationCheckFuns funs
        nl = map fst tl
        bl = map snd tl
        nl2 = [ n | (n,b) <- tl , b == False ]
    in
      case (and bl) of
        True -> trace ("Termination check for " ++ show nl ++ " ok ") True
        False -> case nl of
                    [f] -> trace ("Termination check for function " ++ f ++ " fails ") False
                    _   -> trace ("Termination check for mutual block " ++ show nl ++ " fails for " ++ show nl2) False
terminationCheckDecl _ = True

terminationCheckFuns :: [ (TypeSig,[Clause]) ] -> [(Name,Bool)]
terminationCheckFuns funs = 
    let beh = recBehaviours funs
    in
      zip (map fst beh) (map (checkCalls . snd ) beh )
    where
      checkCalls = (hasLexOrd . toRecBehaviours ) 


flatclause1 = Clause (LHS flatlhs1) (RHS flatrhs1)

flatlhs1 = [ (ConP "nil" [] ) ]
flatrhs1 = Con "nil"

flatclause2 = Clause (LHS flatlhs2) (RHS flatrhs2)

flatlhs2 = [ (ConP "cons" [VarP "l",VarP "ls" ])] 
flatrhs2 = App (Def "aux") [Var "l",Var "ls"]


auxclause1 = Clause (LHS auxlhs1) (RHS auxrhs1)
auxlhs1 = [ (ConP "nil") [] , (VarP "ys") ]
auxrhs1 = App (Def "flat") [Var "ys"]

auxclause2 = Clause (LHS auxlhs2) (RHS auxrhs2)
auxlhs2 = [ (ConP "cons" [VarP "x",VarP "xs"]) , (VarP "ys")]
auxrhs2 = App (Con "cons") [ (Var "x"), App (Def "aux") [Var "xs", Var "ys" ]]

flatfuns =  [(TypeSig "flat" Set,[flatclause1,flatclause2]),
             (TypeSig "aux" Set,[auxclause1,auxclause2])]

flatdecl = FunDecl Ind flatfuns

badfunclauses = [ Clause (LHS [VarP "x"]) (RHS (Var "x"))]

badfun = [(TypeSig "badfun" Set,badfunclauses)] 

--- 

data SemiRing a = SemiRing { add :: (a -> a -> a) , mul :: (a -> a -> a) , one :: a , zero :: a } 

intRing :: SemiRing Integer
intRing = SemiRing { add = (+) , mul = (*), one = 1 , zero = 0 }


------------------------------------------------------------------------
-- Specific semirings

-- | The standard semiring on 'Integer's.

--

type Vector a = [a]

ssum :: SemiRing a -> Vector a -> a
ssum sem v = foldl (add sem) (zero sem) v 

vadd :: SemiRing a -> Vector a -> Vector a -> Vector a
vadd sem v1 v2 = [ (add sem) x y | (x,y) <- zip v1 v2]

scalarProdukt :: SemiRing a -> Vector a -> Vector a -> a
scalarProdukt sem xs ys = ssum sem [(mul sem) x y  | (x,y) <- zip xs ys]

type Matrix a = [Vector a]

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

m1 :: Matrix Integer
m1 = [[1,0,0],[0,2,0],[0,0,3]]

m2 = madd intRing m1 m1

---------------

data Order = Lt
           | Le
           | Un
           deriving (Eq,Show)

instance Ord Order where
    _ <= Lt = True
    Un <= _ = True
    Le <= Le = True
    _ <= _ = False

comp :: Order -> Order -> Order
comp Lt Un = Un
comp Lt  _ = Lt
comp Le o = o
comp Un  _ = Un

supremum :: [Order] -> Order
supremum = foldr max Un

infimum :: [Order] -> Order
infimum = foldr min Lt

ordRing :: SemiRing Order
ordRing = SemiRing { add = max , mul = comp , one = Le , zero = Un }

---


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
      (App _ args) -> concatMap (collectCallsExpr nl f pl) args
                   

        
      (Lam _ e1) -> collectCallsExpr nl f pl e1
      (Pi (TBind _ e1) e2) -> (collectCallsExpr nl f pl e1) ++ 
                              (collectCallsExpr nl f pl e2)
      (Fun e1 e2) ->  (collectCallsExpr nl f pl e1) ++ 
                      (collectCallsExpr nl f pl e2) 
      (Succ e1) -> collectCallsExpr nl f pl e1
      _ -> []


compareArgs :: [Pattern] -> [Expr] -> Matrix Order
compareArgs pl el = map (\ e -> map (compareExpr e) pl ) el

compareExpr :: Expr -> Pattern -> Order
compareExpr e p =  
    case (e,p) of 
      ((Var i),p) -> compareVar i p 
      ((App (Con n) args),(ConP n2 pl)) -> if ( n == n2 ) then
                                               if null args then 
                                                   Le
                                               else
                                                   infimum (zipWith compareExpr args pl)
                                           else
                                               Un
      (Succ e2,SuccP p2) -> compareExpr e2 p2
      _ -> Un

compareVar :: Name -> Pattern -> Order
compareVar n p = 
    case p of
      (VarP n2) -> if n == n2 then Le else Un
      (ConP c pl) -> comp Lt (supremum (map (compareVar n) pl))
      (SuccP p2) -> comp Lt (compareVar n p2)
      (DotP e) -> Un
      _ -> error $ "comparevar " ++ show n ++ "\n" ++ show p
----------------
--lexicrophic ordering

type LexOrder arg = [arg]

type RecBehaviours = Matrix Order

testm1 :: RecBehaviours
testm1 = [ [ Le , Lt , Un ] , [ Le , Le , Lt ] , [ Le , Lt , Le ] ]

testm2 = [ [ Le ]]

testm3 = [ [Lt,Le ] , [Un , Lt ]]

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