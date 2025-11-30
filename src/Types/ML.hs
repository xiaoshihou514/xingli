module Types.ML where

import Typeclasses

-- Definition for the ML type system

data MLType = Phi Char | TyV Char | Basic String | Arrow MLType MLType
  deriving (Show, Eq)

instance Pretty MLType where
  pretty :: MLType -> String
  pretty (Phi c) = [c]
  pretty (TyV c) = ['∀', c]
  pretty (Basic s) = s
  pretty (Arrow a b) = left ++ " -> " ++ pretty b
    where
      left = case a of
        Phi c -> [c]
        _ -> "(" ++ pretty a ++ ")"

(-->) :: MLType -> MLType -> MLType
(-->) = Arrow
