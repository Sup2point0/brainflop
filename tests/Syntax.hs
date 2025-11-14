module Syntax where

import Test.Tasty
import Test.Tasty.HUnit


infix 1 ===
(===) :: (Show t, Eq t, HasCallStack) => t -> t -> Assertion
(===) = (@?=)

infix 1 !==
(!==) :: (Show t, Eq t, HasCallStack) => t -> t -> Assertion
(!==) prot deut = assertBool (show prot ++ " /= " ++ show deut) (prot /= deut)


testCollection :: String -> [Assertion] -> TestTree
testCollection name tests
  = testGroup name (zipWith (\n -> testCase ("#" ++ show n)) [1..] tests)
