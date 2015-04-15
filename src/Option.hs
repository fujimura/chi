module Option
  ( buildOption
  ) where

import           CommandLineOption   (CommandLineOption)
import qualified CommandLineOption
import qualified Git
import           Types

import           Control.Applicative
import           Data.Char           (toUpper)
import           Data.Maybe          (catMaybes, fromJust, fromMaybe)
import           Data.Time.Calendar  (toGregorian)
import           Data.Time.Clock     (getCurrentTime, utctDay)

buildOption :: CommandLineOption -> IO Option
buildOption copt = do
    let packageName'   = CommandLineOption.packageName copt
        moduleName'    = fromMaybe (modularize packageName') (CommandLineOption.moduleName copt)
        directoryName' = fromMaybe packageName' (CommandLineOption.directoryName copt)
        source'        = fromJust $ (Repo <$> CommandLineOption.repo copt) <|> (CabalPackage <$> CommandLineOption.cabalPackage copt)
        afterCommands' = catMaybes [ CommandLineOption.afterCommand copt ]
        dryRun'        = CommandLineOption.dryRun copt

    year'   <- getCurrentYear
    author' <- Git.config "user.name"
    email'  <- Git.config "user.email"
    return Option { packageName   = packageName'
                  , moduleName    = moduleName'
                  , directoryName = directoryName'
                  , author        = author'
                  , email         = email'
                  , year          = year'
                  , source        = source'
                  , dryRun        = dryRun'
                  , afterCommands = afterCommands'
                  }

getCurrentYear :: IO String
getCurrentYear  = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return $ show y

-- | Capitalize words and connect them with periods
--
-- >>> modularize "package"
-- "Package"
--
-- >>> modularize "package-name"
-- "Package.Name"
--
-- >>> modularize "another-package-name"
-- "Another.Package.Name"
--
modularize :: String -> String
modularize []     = []
modularize [x]    = [toUpper x]
modularize (x:xs) = toUpper x : rest xs
  where
    rest []       = []
    rest ('-':ys) = '.' : modularize ys
    rest (y:ys)   = y:rest ys
