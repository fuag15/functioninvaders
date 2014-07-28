module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  hits <- hlint $ ["src"] ++ args
  unless (null hits) exitFailure
