module Test.Ch03.IsomorphismSpec (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import Ch03.Isomorphism

-- Simple unit tests
testInvertPositive :: Test
testInvertPositive = testCase "invert positive temperature" $
    toInt (invert (Temp 10)) @?= (-10)

testInvertNegative :: Test  
testInvertNegative = testCase "invert negative temperature" $
    toInt (invert (Temp (-5))) @?= 5

testInvertZero :: Test
testInvertZero = testCase "invert zero temperature" $
    toInt (invert (Temp 0)) @?= 0

tests :: [Test]
tests = 
  [ testGroup "Unit Tests"
    [ testInvertPositive
    , testInvertNegative
    , testInvertZero
    ]
  ]