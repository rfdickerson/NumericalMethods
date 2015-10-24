{-# LANGUAGE Haskell2010, TemplateHaskell #-}

import Control.Monad

import NumericalMethods.Simple

import Test.QuickCheck
import Test.QuickCheck.All

import System.Exit

mysqrttest m guess = sqrt m

main = do

    success <- $(quickCheckAll)

    (if success then exitSuccess else exitFailure)
