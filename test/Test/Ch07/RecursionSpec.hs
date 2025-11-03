module Test.Ch07.RecursionSpec (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import Ch07.Recursion

-- Simple recursion tests
testNatToInt :: Test
testNatToInt = testCase "natToInt" $
    natToInt (S (S Z)) @?= (2 :: Int)

testPlus :: Test
testPlus = testCase "plus function" $
    natToInt (plus (S Z) (S Z)) @?= (2 :: Int)

testFoldrC :: Test
testFoldrC = testCase "foldrC sum" $
    foldrC (+) (0 :: Int) [1, 2, 3] @?= (6 :: Int)

tests :: [Test]
tests = 
  [ testGroup "Recursion Tests"
    [ testNatToInt
    , testPlus
    , testFoldrC
    ]
  ]