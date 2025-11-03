module Main where

import Test.Framework (defaultMain, testGroup, Test)

import qualified Test.Ch03.IsomorphismSpec as Ch03
import qualified Test.Ch04.SumSpec as Ch04  
import qualified Test.Ch06.FunctionSpec as Ch06
import qualified Test.Ch07.RecursionSpec as Ch07

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = 
  [ testGroup "Ch03.Isomorphism Tests" Ch03.tests
  , testGroup "Ch04.Sum Tests" Ch04.tests  
  , testGroup "Ch06.Function Tests" Ch06.tests
  , testGroup "Ch07.Recursion Tests" Ch07.tests
  ]