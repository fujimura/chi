module CommandLineOption
  ( CommandLineOption(..)
  , commandLineOption
  ) where

import           Options.Applicative

data CommandLineOption = CommandLineOption
                       { packageName   :: String
                       , moduleName    :: Maybe String
                       , directoryName :: Maybe FilePath
                       , repo          :: Maybe String
                       , cabalPackage  :: Maybe String
                       , afterCommand  :: Maybe String
                       }

commandLineOption :: Parser CommandLineOption
commandLineOption = CommandLineOption
   <$> argument str (help "Package name")
   <*> optional (strOption (short 'm' <> long "module-name"    <> help "Name of Module"))
   <*> optional (strOption (short 'd' <> long "directory-name" <> help "Directory to generate file"))
   <*> optional (strOption (short 'r' <> long "repository"     <> help "Repository of template"))
   <*> optional (strOption (short 'c' <> long "cabal-package"  <> help "Name of cabal package"))
   <*> optional (strOption (             long "after-command"  <> help "Command to run after generation"))
