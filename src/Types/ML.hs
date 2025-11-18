module Types.ML where

import Typeclasses

-- Definition for the ML type system

data MLType = Phi Char | Forall Char | IntType | BoolType | Arrow MLType MLType
  deriving (Show, Eq)

instance Pretty MLType where
  pretty :: MLType -> String
  pretty (Phi c) = [c]
  pretty (Forall c) = ['∀', c]
  pretty IntType = "Int"
  pretty BoolType = "Bool"
  pretty (Arrow a b) = left ++ " -> " ++ pretty b
    where
      left = case a of
        Phi c -> [c]
        _ -> "(" ++ pretty a ++ ")"

(-->) :: MLType -> MLType -> MLType
(-->) = Arrow
