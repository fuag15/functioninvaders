{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Char
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import NFInvaders.Util

-- dropping the "Hello mr " string is the same as identity
prop_sayHi name = drop (length "Hello mr. ") (sayHi name) == id name

main :: IO ()
main = $defaultMainGenerator
