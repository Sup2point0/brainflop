module Brainflop where

import Engine.Parser (parseFully)
import Brainflop.Instr
import Brainflop.Parser (brainflopParser)
import Brainflop.Executor (exec)


parse :: String -> [Instr]
parse prog = head $ parseFully brainflopParser prog
