{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Util where

import Prelude hiding (showList)

import Control.Applicative hiding (empty)
import Control.Monad.Writer (Writer, runWriter, All, getAll)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Text.PrettyPrint as PP

(+?+) :: String -> String -> String
(+?+) xs "[]" = []
(+?+) xs ys = xs ++ ys

implies :: Bool -> Bool -> Bool
implies a b = if a then b else True

class Pretty a where
    pretty	:: a -> Doc
    prettyPrec	:: Int -> a -> Doc

    pretty	= prettyPrec 0
    prettyPrec	= const pretty

instance Pretty Doc where
    pretty = id

angleBrackets :: Doc -> Doc
angleBrackets d = text "<" <+> d <+> text ">"

parensIf :: Bool -> Doc -> Doc
parensIf b = if b then PP.parens else id

hsepBy :: Doc -> [Doc] -> Doc
hsepBy sep [] = empty
hsepBy sep [d] = d
hsepBy sep (d:ds) = d <> sep <> hsepBy sep ds

pwords :: String -> [Doc]
pwords = map text . words

fwords :: String -> Doc
fwords = fsep . pwords

fromAllWriter :: Writer All a -> (Bool, a)
fromAllWriter m = let (a, w) = runWriter m
                  in  (getAll w, a)

traceM :: (Monad m) => String -> m ()
traceM msg = trace msg $ return ()

liftMaybe :: (Monad m) => Maybe a -> m a
liftMaybe = maybe (fail "Util.liftMaybe: unexpected Nothing") return

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) k = k a
whenJust Nothing  k = return ()

whenNothing :: (Monad m) => Maybe a -> m () -> m ()
whenNothing Nothing m = m
whenNothing Just{}  m = return ()

lookupM :: (Monad m, Show k, Ord k) => k -> Map k v -> m v
lookupM k m = maybe (fail $ "lookupM: unbound key " ++ show k) return $ Map.lookup k m

mapMapM :: (Monad m, Ord k) => (a -> m b) -> Map k a -> m (Map k b)
mapMapM f = Map.foldrWithKey step (return $ Map.empty)
  where step k a m = do a' <- f a
                        m' <- m
                        return $ Map.insert k a' m'

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c d e = do { b <- c ; if b then d else e }

{- Control.Monad.IfElse
whenM :: Monad m => m Bool -> m () -> m ()
whenM c d = do { b <- c; if b then d else return () }

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c e = do { b <- c; if b then return () else e }
-}

andLazy :: Monad m => m Bool -> m Bool -> m Bool
andLazy ma mb = ifM ma mb $ return False

andM  :: Monad m => [m Bool] -> m Bool
andM []     = return True
andM (m:ms) = m `andLazy` andM ms

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p []       = return Nothing
findM p (x : xs) = do b <- p x
                      if b then return (Just x) else findM p xs

-- | Binary version of @=<<@.
(==<<) :: Monad m => (a -> b -> m c) -> (m a, m b) -> m c
f ==<< (ma, mb) = do { a <- ma; f a =<< mb }

parens :: String -> String
parens s = "(" ++ s ++ ")"

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

bracketsIf :: Bool -> String -> String
bracketsIf False s = s
bracketsIf True  s = "[" ++ s ++ "]"

separate :: String -> String -> String -> String
separate sep "" y = y
separate sep x "" = x
separate sep x y  = x ++ sep ++ y

showList :: String -> (a -> String) -> [a] -> String
showList sep f [] = ""
showList sep f [e] = f e
showList sep f (e:es) = f e ++ sep ++ showList sep f es
-- OR: showList sep f es = foldl separate "" $ map f es

hasDuplicate :: (Eq a) => [a] -> Bool
hasDuplicate [] = False
hasDuplicate (x : xs) = x `elem` xs || hasDuplicate xs

compressMaybes :: [Maybe a] -> [a]
compressMaybes = concat . map (maybe [] (\ a -> [a]))

mapFst :: (a -> c) -> (a,d) -> (c,d)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> d) -> (a,b) -> (a,d)
mapSnd f (a,b) = (a, f b)

mapPair :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
mapPair f g (a,b) = (f a, g b)

zipPair :: (a -> b -> c) -> (d -> e -> f) -> (a,d) -> (b,e) -> (c,f)
zipPair f g (a,d) (b,e) = (f a b, g d e)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a:as) = Just a

headM :: Monad m => [a] -> m a
headM [] = fail "headM"
headM (a:as) = return a

firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (mm : mms) = do
  m <- mm
  case m of
    Nothing -> firstJustM mms
    Just{}  -> return m

mapOver :: (Functor f) => f a -> (a -> b) -> f b
mapOver = flip fmap

mapAssoc :: (a -> b) -> [(n,a)] -> [(n,b)]
mapAssoc f = map (\ (n, a) -> (n, f a))

mapAssocM :: (Applicative m, Monad m) => (a -> m b) -> [(n,a)] -> m [(n,b)]
mapAssocM f = mapM (\ (n, a) -> (n,) <$> f a)

compAssoc :: Eq b => [(a,b)] -> [(b,c)] -> [(a,c)]
compAssoc xs ys = [ (a,c) | (a,b) <- xs, (b',c) <- ys, b == b' ]

{- this is Traversable.mapM
fmapM :: (Functor f) => (a -> m b) -> f a -> m (f b)
fmapM = Traversable.traverse
-}
