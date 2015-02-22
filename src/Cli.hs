module Cli
  ( run
  ) where

import qualified Chi
import           CommandLineOption   (commandLineOption)
import           Option              (buildOption)
import           Types

import qualified Paths_chi           (version)

import           Control.Applicative
import           Data.Version        (showVersion)
import           Options.Applicative

run :: [String] -> IO ()
run args = parseArgs args >>= Chi.run

parseArgs :: [String] -> IO Option
parseArgs args = handleParseResult (execParserPure (prefs idm) opts args) >>= buildOption
  where
    opts = info (helper <*> (version <*> commandLineOption))
      ( fullDesc
      <> header "chi - Generate scaffold for a Haskell project")
    version = infoOption (showVersion Paths_chi.version)
      (  short 'v'
      <> long "version"
      <> help "Print version information" )
