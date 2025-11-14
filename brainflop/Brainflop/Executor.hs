module Brainflop.Executor where

import Data.Char qualified as Char
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Strict (IntMap, (!))
import Control.Monad

import Brainflop.Instr qualified as Instr
import Brainflop.Instr (Instr)

import Debug.Trace


type Pointer = Int
type Registers = IntMap Int


exec :: [Instr] -> IO (Pointer, Registers)
exec prog
    = process prog 0 IntMap.empty
  where
    process :: [Instr] -> Pointer -> Registers -> IO (Pointer, Registers)
    process [] ptr regs = return (ptr, regs)

    process (instr:rest) ptr regs = do
        !(ptr', regs') <- exec' instr ptr regs
        process rest ptr' regs'
    
    exec' :: Instr -> Pointer -> Registers -> IO (Pointer, Registers)
    exec' instr ptr regs
        = case instr of
            Instr.RIGHT -> return (ptr + 1, regs)
            Instr.LEFT  -> return (ptr - 1, regs)
            Instr.PLUS  -> return (ptr    , IntMap.insertWith (+) ptr 1 regs)
            Instr.MINUS -> return (ptr    , IntMap.insertWith (+) ptr (-1) regs)
            
            Instr.PRINT -> do
              putStr $ [Char.chr (regs ! ptr)]
              return (ptr, regs)
            
            Instr.LOOP inner -> loop inner ptr regs
      where
        loop :: [Instr] -> Pointer -> Registers -> IO (Pointer, Registers)
        loop [] ptr regs = return (ptr, regs)

        loop prog ptr regs = do
          if (regs ! ptr) == 0 then
            return (ptr, regs)
          else do
            (ptr', regs') <- process prog ptr regs
            loop prog ptr' regs'
