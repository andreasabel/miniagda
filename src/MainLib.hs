module MainLib where

import Prelude hiding (null, mapM_)

import Data.Foldable (mapM_)

import System.Exit
import System.IO (stdout, hSetBuffering, BufferMode(..))

import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Pretty as H

import Lexer
import Parser
import Options

import qualified Concrete as C
import qualified Abstract as A
import Abstract (Name)
import ScopeChecker
import TCM
import TypeChecker
import Extract
import ToHaskell

import Util

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- putStrLn "MiniAgda by Andreas Abel and Karl Mehltretter"
  opts <- options
  mapM_ (mainFile $ optControlUnfolding opts) $ optInputs opts

mainFile
  :: Bool      -- ^ Option @--control-unfolding@ on?
  -> FilePath  -- ^ File to check.
  -> IO ()
mainFile controlUnfolding fileName = do
  putStrLn $ "--- opening " ++ show fileName ++ " ---"
  file <- readFile fileName
  let t = alexScanTokens file
  let cdecls =  parse t
  -- putStrLn "--- parsing ---"
  -- mapM (putStrLn . show) cdecls
  putStrLn "--- scope checking ---"
  adecls <- doScopeCheck cdecls
  -- mapM (putStrLn . show) adecls
  putStrLn "--- type checking ---"
  (edecls, sig) <- doTypeCheck controlUnfolding adecls
  putStrLn "--- evaluating ---"
  showAll sig adecls
{-
  putStrLn "--- extracting ---"
  edecls <- doExtract sig edecls
  hsmodule <- doTranslate edecls
  putStrLn $ H.prettyPrint hsmodule
  -- printHsDecls hsdecls
-}
  putStrLn $ "--- closing " ++ show fileName ++ " ---"

-- print extracted program

ppHsMode :: H.PPHsMode
ppHsMode = H.PPHsMode  -- H.defaultMode
  { H.classIndent  = 2
  , H.doIndent     = 3
  , H.multiIfIndent = 3
  , H.caseIndent   = 3
  , H.letIndent    = 4
  , H.whereIndent  = 2
  , H.onsideIndent = 1
  , H.spacing      = False
  , H.layout       = H.PPOffsideRule
  , H.linePragmas  = False
  }

printHsDecls :: [H.Decl ()] -> IO ()
printHsDecls hs = mapM_ (putStrLn . H.prettyPrintWithMode ppHsMode) hs

-- all let declarations
allLet :: Signature -> [A.Declaration] -> [(Name,A.Expr)]
allLet sig [] = []
allLet sig (decl:xs) =
    case decl of
      (A.LetDecl True n tel _ _ e) | null tel ->
          (n,e):(allLet sig xs)
      _ -> allLet sig xs


showAll :: Signature -> [A.Declaration] -> IO ()
showAll sig decl = mapM_ (showLet sig) $ allLet sig decl

showLet :: Signature -> (Name,A.Expr) -> IO ()
showLet sig (n,e) = do
  r <- doWhnf sig e
  case r of
    Right (v,_) -> putStrLn $ show n ++ " has whnf " ++ show v
    Left err    -> do putStrLn $ "error during evaluation:\n" ++ show err
                      exitFailure
  r <- doNf sig e
  case r of
    Right (v,_) -> putStrLn $ show n ++ " evaluates to " ++ show v
    Left err    -> do putStrLn $ "error during evaluation:\n" ++ show err
                      exitFailure

doExtract :: Signature -> [A.EDeclaration] -> IO [A.EDeclaration]
doExtract sig decls = do
  k <- runExtract sig $ extractDecls decls
  case k of
    Left err -> do
      putStrLn $ "error during extraction:\n" ++ show err
      exitFailure
    Right (hs, _) ->
      return hs

doTranslate :: [A.EDeclaration] -> IO (H.Module ())
doTranslate decls = do
  k <- runTranslate $ translateModule decls
  case k of
    Left err -> do
      putStrLn $ "error during extraction:\n" ++ show err
      exitFailure
    Right hs ->
      return hs

doTypeCheck
  :: Bool             -- ^ Control unfolding of definitions?
  -> [A.Declaration]  -- ^ Scope-checked declarations to type check.
  -> IO ([A.EDeclaration], Signature)
doTypeCheck controlUnfolding decls = do
  typeCheck controlUnfolding decls >>= \case
    Left err -> do
      putStrLn $ "error during typechecking:\n" ++ show err
      exitFailure
    Right (edecls, st) ->
      return (edecls, signature st)

doScopeCheck :: [C.Declaration] -> IO [A.Declaration]
doScopeCheck decl =
  case scopeCheck decl of
    Left err -> do
      putStrLn $ "scope check error: " ++ show err
      exitFailure
    Right (decl',_) -> do
      return decl'
