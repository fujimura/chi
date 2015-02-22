{-# LANGUAGE ScopedTypeVariables #-}

module Git
    (
      clone
    , config
    , lsFiles
    ) where

import           Control.Applicative
import           Control.Exception
import           System.Exit
import           System.Process      (callCommand, readProcess)

-- | Clone given repository to current directory
clone :: String -> IO ()
clone repoUrl = handle (\(e :: IOException) -> print e >> exitFailure) $ do
    callCommand $ "git clone --no-checkout --quiet " ++ repoUrl ++ " " ++ "./"
    callCommand "git checkout HEAD --quiet"

-- | Return file list by `git ls-files`
lsFiles :: IO [String]
lsFiles = lines <$> readProcess "git" ["ls-files"] []

-- | Return given config value
config :: String -> IO String
config name = removeNewline <$> readProcess "git" ["config", name] []

removeNewline :: String -> String
removeNewline = reverse . dropWhile (=='\n') . reverse
