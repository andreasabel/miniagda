{-# LANGUAGE CPP #-}

-- | Options given to @MiniAgda@.

module Options where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup               ( (<>) )
#endif

import Options.Applicative
import Options.Applicative.NonEmpty ( some1 )

import System.Environment           ( getArgs )

import License                      ( copyright, license )
import Util
import Version                      ( version )

-- | Options given to @MiniAgda@.
data Options = Options
  { optControlUnfolding :: Bool
      -- ^ Control unfolding of constants?
  , optInputs :: List1 FilePath
      -- ^ The source files to type check, from first to last.
  }

self :: String
self = "MiniAgda"

-- | Parse the options given on the command line.
options :: IO Options
options = handleParseResult =<< do parseOptions <$> getArgs

-- | Pure parser for command line given by a list of strings.
parseOptions :: [String] -> ParserResult Options
parseOptions = execParserPure style $ info parser description
  where
  parser = theOptions <**>
    (versionOption <*> numericVersionOption <*> licenseOption <*> helper)
  style  = prefs $ multiSuffix "..."

  description = header top <> footer bot
  top = unwords
    [ concat [ self, ":" ]
    , "A prototypical dependently typed pure language with sized types."
    ]
  bot = unwords
    [ "Checks the given MiniAgda files in first-to-last order."
    -- , "Later files can refer to definitions made in earlier files."
    ]

  versionOption =
    infoOption versionLong
      $  long "version"
      <> short 'V'
      <> help "Show version info."
      <> hidden
  versionText = unwords [ self, "version", version ]
  versionLong = intercalate "\n" $
    [ versionText
    , copyright
    , "This is free software under the MIT license."
    ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> help "Show just version number."
      <> hidden

  licenseOption =
    infoOption license
      $  long "license"
      <> help "Show the license text."
      <> hidden

  theOptions = Options
    <$> oControlUnfolding
    <*> oInputs

  oControlUnfolding :: Parser Bool
  oControlUnfolding =
    switch
      $  long "control-unfolding"
      <> help "Enable user-controlled unfolding of definitions."

  oInputs :: Parser (List1 FilePath)
  oInputs = some1 $
    strArgument
      $  metavar "FILE"
      <> action "file"
      <> help "MiniAgda file to check."
      -- <> help "Files to check (in first-to-last order)."
