module Test.Ch06.FunctionSpec (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import Ch06.Function

-- Simple function tests
testCurry :: Test
testCurry = testCase "curry function" $
    Ch06.Function.curry (\(x, y) -> x + y) (5 :: Int) (3 :: Int) @?= (8 :: Int)

testUncurry :: Test
testUncurry = testCase "uncurry function" $
    Ch06.Function.uncurry (*) ((4 :: Int), (7 :: Int)) @?= (28 :: Int)

testMapOutLeft :: Test
testMapOutLeft = testCase "mapOut on Left" $
    mapOut ((*2), (+10)) (Left (5 :: Int)) @?= (10 :: Int)

tests :: [Test]
tests = 
  [ testGroup "Function Tests"
    [ testCurry
    , testUncurry
    , testMapOutLeft
    ]
  ]