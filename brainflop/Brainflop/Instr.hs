module Brainflop.Instr where

import Data.Set


_INSTR_CHARS_ :: Set Char
_INSTR_CHARS_ = fromList
  [ '>'
  , '<'
  , '+'
  , '-'
  , '.'
  , ','
  , '['
  , ']'
  ]


data Instr
    = RIGHT | LEFT
    | PLUS  | MINUS
    | PRINT | READ
    | LOOP [Instr]
  deriving Eq

instance Show Instr where
  show RIGHT = " > "
  show LEFT  = " < "
  show PLUS  = " + "
  show MINUS = " - "
  show PRINT = " PRINT "
  show READ = " READ "
  show (LOOP prog) = " LOOP {" ++ show prog ++ "} "
