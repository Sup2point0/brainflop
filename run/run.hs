import Data.List
import Data.Map qualified as Map

import Brainflop


main :: IO ()
main = do
    putStrLn "\n[Enter your Brainf*ck program]:"
    prog <- getLine
    putStrLn ""

    (_, regs) <- (exec (parse prog))
    showRegisters regs
  where
    showRegisters :: Registers -> IO ()
    showRegisters regs = do
      putStr "\n[ "
      putStr (
          intercalate ", " $ map (show . snd) (Map.toList regs)
        )
      putStr " ]"
