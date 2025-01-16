{-# LANGUAGE OverloadedStrings #-}

import FrequentSubstring
import Util (showText)

import Data.Text
import Test.HUnit

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList
  [ TestLabel "test1" test1
  , TestLabel "test2" test2
  , TestLabel "test3" test3
  ]

test3 :: Test
test3 = TestCase $ do
  let t = "aababcabcdabcdeabcdefabcdefgabcdefgh"
  let expected = ("abcd", 5)
  let r = 5
  let actual = longestSubstringWithRRepeats r t
  assertEqual (unpack $ "longestSubstringWithRRepeats r=" <> showText r) expected actual

test2 :: Test
test2 = TestCase $ do
  let t = "abbcccddddeeeeeffffffggggggghhhhhhhh"
  let expected = ("hhhhh", 4)
  let r = 4
  let actual = longestSubstringWithRRepeats r t
  assertEqual (unpack $ "longestSubstringWithRRepeats r=" <> showText r) expected actual

test1 :: Test
test1 = TestCase $ do
  let t = "abbcccddddeeeeeffffffggggggghhhhhhhh"
  let expected = ("h", 8)
  let r = 8
  let actual = longestSubstringWithRRepeats r t
  assertEqual (unpack $ "longestSubstringWithRRepeats r=" <> showText r) expected actual



