module Main
  ( main
  ) where

import qualified Cli

import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= Cli.run
