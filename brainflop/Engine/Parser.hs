module Engine.Parser where

import Data.Set qualified as Set
import Data.Maybe
import Control.Applicative hiding (many, some)

import Brainflop.Instr


data Parser a = Parser {
    parse :: String -> [(a, String)]
  }
  deriving Functor


instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\str -> [(x, str)])

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 f p q
    = Parser $ \str ->
        [ (f r s, x') | (r, x)  <- parse p str
                      , (s, x') <- parse q x
                      ]

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const [])

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser {
      parse = \str -> parse p str ++ parse q str
    }


-- (<$) :: Functor funct => a -> funct b -> funct a
-- x <$ fy = const x <$> fy


satisfies :: (Char -> Bool) -> Parser Char
satisfies f = Parser eat
  where
    eat :: String -> [(Char, String)]
    eat (c:cs)
      | f c = [(c, cs)]
    eat _ = []


many :: Parser a -> Parser [a]
many p = some p <|> pure []

some :: Parser a -> Parser [a]
some p = liftA2 (:) p (many p)


any_char :: Parser Char
any_char = satisfies (const True)

char :: Char -> Parser Char
char c = satisfies (== c)

ignore :: Parser Char
ignore = satisfies (\c -> not $ Set.member c _INSTR_CHARS_)


parseFully :: Parser a -> String -> [a]
parseFully p str
  = [result | (result, "") <- parse p str]
