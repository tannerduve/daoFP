module Test.Ch04.SumSpec (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import Ch04.Sum

-- Simple boolean function tests
testNotTrue :: Test
testNotTrue = testCase "not True" $ 
    Ch04.Sum.not True @?= False

testNotFalse :: Test  
testNotFalse = testCase "not False" $ 
    Ch04.Sum.not False @?= True

testCommLeft :: Test
testCommLeft = testCase "comm on Left" $
    comm (Left 'a' :: Either Char Int) @?= (Right 'a' :: Either Int Char)

testCommRight :: Test  
testCommRight = testCase "comm on Right" $
    comm (Right (42 :: Int) :: Either Char Int) @?= (Left (42 :: Int) :: Either Int Char)

tests :: [Test]
tests = 
  [ testGroup "Boolean Functions"
    [ testNotTrue
    , testNotFalse  
    ]
  , testGroup "Either Functions"
    [ testCommLeft
    , testCommRight
    ]
  ]