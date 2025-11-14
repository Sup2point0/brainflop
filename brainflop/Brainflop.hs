module Brainflop (
    parse,
    exec,
    Pointer,
    Registers
  ) where

import Engine.Parser (parseFully)
import Brainflop.Instr
import Brainflop.Parser (brainflopParser)
import Brainflop.Executor (exec, Pointer, Registers)


parse :: String -> [Instr]
parse prog = head $ parseFully brainflopParser prog
