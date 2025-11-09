module Typeclasses where

class Pretty a where
  pretty :: a -> String

class FromPretty a where
  parse :: String -> a
