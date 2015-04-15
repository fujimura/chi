module Types where

data Option = Option
             { packageName   :: String
             , moduleName    :: String
             , directoryName :: String
             , author        :: String
             , email         :: String
             , year          :: String
             , source        :: Source
             , dryRun        :: Bool
             , afterCommands :: [String]
             } deriving (Eq,Ord,Show)

type File = (FilePath,String)

-- Data tagged as original.
data Original a = Original a deriving (Eq,Ord,Show)

-- Modified data contains original.
data Modified a = Modified a (Original a) deriving (Eq,Ord,Show)

-- Source of the package.
data Source = Repo String | CabalPackage String deriving (Eq,Ord,Show)
