{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Char
import Data.List
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)
import NFInvaders.Util

case_sayHi = do "Hello mr. bitch" @=? (sayHi "bitch")

main :: IO ()
main = defaultMain [$testGroupGenerator]
