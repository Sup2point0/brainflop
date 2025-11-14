import Test.Tasty
import Test.Tasty.HUnit

import System.Exit
import Data.IntMap.Strict qualified as IntMap
import Control.Exception
import Control.Monad

import Syntax (testCollection, (===), (!==))
import Utils (testProg)

import Brainflop
import Brainflop.Instr qualified as Instr


main :: IO ()
main = defaultMain tests
  `catch` (\e -> do
    if e == ExitSuccess then
      io_tests
    else
      throwIO e
  )

tests :: TestTree
tests = testGroup "brainflop"
  [ testCollection "parser"   $ test_parse
  ]

io_tests :: IO ()
io_tests = do
  putStrLn "\nExecuting IO tests...\n"
  test_exec


test_parse :: [Assertion]
test_parse =
  [ parse "" === []
  , parse "+" === [Instr.PLUS]
  ]

test_exec :: IO ()
test_exec = do
  testProg "1 + 2 == 3" (
    do
      (ptr, res) <- exec $ parse "+>++<[->+<]"
      return (res == IntMap.fromList [(0, 0), (1, 3)])
    )
  
  testProg "Hello, World!" (
    do
      (ptr, res) <- exec $ parse (
          "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
        )
      return (res == IntMap.fromList [(0, 0), (1, 0), (2, 72), (3, 100), (4, 87), (5, 33), (6, 10)])
    )
