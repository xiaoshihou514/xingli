-- kill me
{-# OPTIONS_GHC -Wno-orphans #-}

module Parser.LCN where

import Expr.LCN
import Parser.Applicative
import Typeclasses

instance FromPretty LCNTerm where
  parse :: String -> LCNTerm
  parse input = case Parser.Applicative.parse parser input of
    [(lcnt, "")] -> lcnt
    _ -> error "Broken input"
    where
      parser :: Parser a
      parser = undefined
