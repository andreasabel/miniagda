{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyTCM where

import Prelude hiding (sequence, mapM, (<>))
import Data.String (IsString(..))

import Abstract
import {-# SOURCE #-} Eval
import {-# SOURCE #-} TCM
import qualified Util
import Value

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad ((<=<))
import Data.Traversable

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Text.PrettyPrint as P


-- from Agda.TypeChecking.Pretty

type Doc = P.Doc

empty, comma, colon :: Monad m => m Doc

empty      = return P.empty
comma      = return P.comma
colon      = text ":"
pretty x   = return $ Util.pretty x
-- prettyA x  = P.prettyA x
text s     = return $ P.text s
pwords s   = map return $ Util.pwords s
fwords s   = return $ Util.fwords s
sep ds     = P.sep <$> sequence ds
fsep ds    = P.fsep <$> sequence ds
hsep ds    = P.hsep <$> sequence ds
vcat ds    = P.vcat <$> sequence ds
d1 $$ d2   = (P.$$) <$> d1 <*> d2
d1 <> d2   = (P.<>) <$> d1 <*> d2
d1 <+> d2  = (P.<+>) <$> d1 <*> d2
nest n d   = P.nest n <$> d
braces d   = P.braces <$> d
brackets d = P.brackets <$> d
parens d   = P.parens <$> d

prettyList ds = brackets $ fsep $ punctuate comma ds

punctuate _ [] = []
punctuate d ds = zipWith (<>) ds (replicate n d ++ [empty])
    where
        n = length ds - 1

instance Monad m => IsString (m Doc) where
  fromString = text

-- monadic pretty printing

class ToExpr a where
  toExpression :: a -> TypeCheck Expr

instance ToExpr Expr where
  toExpression = return

instance ToExpr Val where
  toExpression = toExpr


class PrettyTCM a where
  prettyTCM :: a -> TypeCheck Doc

instance PrettyTCM Name where
  prettyTCM = pretty

instance PrettyTCM [Name] where
  prettyTCM = sep . map pretty

instance PrettyTCM a => PrettyTCM (Set a) where
  prettyTCM = sep . map prettyTCM . Set.toList

instance PrettyTCM Pattern where
  prettyTCM = pretty

instance PrettyTCM [Pattern] where
  prettyTCM = sep . map pretty

instance PrettyTCM Expr where
  prettyTCM = pretty

instance PrettyTCM (Sort Expr) where
  prettyTCM = pretty

instance PrettyTCM Val where
  prettyTCM = pretty <=< toExpr

instance PrettyTCM [Val] where
  prettyTCM = sep . map (pretty <=< toExpr)

instance PrettyTCM (Sort Val) where
  prettyTCM = pretty <=< mapM toExpr

instance PrettyTCM a => PrettyTCM (OneOrTwo a) where
  prettyTCM (One a)     = prettyTCM a
  prettyTCM (Two a1 a2) = prettyTCM a1 <+> text "||" <+> prettyTCM a2

instance (ToExpr a) => PrettyTCM (Measure a) where
  prettyTCM mu = pretty =<< mapM toExpression mu

instance (ToExpr a) => PrettyTCM (Bound a) where
  prettyTCM beta = pretty =<< mapM toExpression beta

instance (PrettyTCM a, PrettyTCM b) => PrettyTCM (a,b) where
  prettyTCM (a,b) = parens $ prettyTCM a <> comma <+> prettyTCM b
