module Brainflop.Parser (
    brainflopParser
  ) where

import Data.Maybe
import Control.Applicative hiding (many, some)

import Brainflop.Instr qualified as Instr
import Brainflop.Instr (Instr)
import Engine.Parser


instr :: Parser (Maybe Instr)
instr =
      Nothing          <$ ignore
  <|> Just Instr.RIGHT <$ char '>'
  <|> Just Instr.LEFT  <$ char '<'
  <|> Just Instr.PLUS  <$ char '+'
  <|> Just Instr.MINUS <$ char '-'
  <|> Just Instr.PRINT <$ char '.'
  <|> Just Instr.READ  <$ char ','
  <|> char '[' *> (Just . Instr.LOOP <$> brainflopParser ) <* char ']'


brainflopParser :: Parser [Instr]
brainflopParser = catMaybes <$> many instr
