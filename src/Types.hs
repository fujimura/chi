module Types where

data Option = Option
             { packageName   :: String
             , moduleName    :: String
             , directoryName :: String
             , author        :: String
             , email         :: String
             , year          :: String
             , source        :: Source
             , afterCommands :: [String]
             } deriving (Eq,Ord,Show)

type File = (FilePath,String)

data Original a = Original a deriving(Eq,Ord,Show)
data Modified a = Modified a (Original a) deriving (Eq,Ord,Show) -- TODO No way to distinguish modified/original

data Source = Repo String | CabalPackage String deriving (Eq,Ord,Show)
