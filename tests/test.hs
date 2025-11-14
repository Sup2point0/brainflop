import Test.Tasty
import Test.Tasty.HUnit

import Data.Map qualified as Map
import Control.Monad

import Syntax (testCollection, (===), (!==))

import Brainflop


main :: IO ()
main = do
  test_exec
  defaultMain tests

tests :: TestTree
tests = testGroup "brainflop"
  [ testCollection "parser"   $ test_parse
  -- , testCollection "executor" $ test_exec
  ]


test_parse :: [Assertion]
test_parse =
  []

test_exec :: IO ()
test_exec = do
  putStrLn "> Testing 1 + 2 == 3"
  check <- do
    (ptr, res) <- exec $ parse "+>++<[->+<]"
    return (res == Map.fromList [(0, 0), (1, 3)])
  guard check
  
  putStrLn "> Testing Hello, World!"
  check <- do
    (ptr, res) <- exec $ parse (
        "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
      )
    return (res == Map.fromList [(0, 0), (1, 0), (2, 72), (3, 104), (4, 88), (5, 32), (6, 8)])
  guard check
