module Parser.Applicative where

import Control.Applicative

newtype Parser a = Parser (String -> [(a, String)]) deriving (Functor)

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser eat
  where
    eat :: String -> [(Char, String)]
    eat (c : cs)
      | f c = [(c, cs)]
    eat _ = []

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\cs -> [(x, cs)]) -- consume no input, produce `x`

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 f p q = Parser $ \cs ->
    -- we have some input `cs`, we need to run `p` first on `cs`
    -- then pass the leftovers to `q` to get a final residual and the two
    -- results to combine with `f`. Let's use a list comprehension!
    [ (f x y, cs'')
    | (x, cs') <- parse p cs,
      (y, cs'') <- parse q cs'
    ]

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const [])

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser $ \cs ->
    parse p cs ++ parse q cs

many :: Parser a -> Parser [a]
many p = some p <|> pure []
