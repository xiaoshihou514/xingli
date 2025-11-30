module Types.ML where

import Typeclasses

-- Definition for the ML type system

data MLType
  = -- Note that phi here is actually "generic", ie type variable
    Phi Char
  | -- Int, String...
    Basic String
  | Arrow MLType MLType
  | -- Pre condition: Qtf can only exist on the outmost level
    -- Else it's system F, which is undecidable
    Qtf Char MLType
  deriving (Show, Eq)

-- Remark: a -> Int is a valid type even though a is
-- not quantified in this context

instance Pretty MLType where
  pretty :: MLType -> String
  pretty (Phi c) = [c]
  pretty (Basic s) = s
  pretty (Qtf c ty) = ['∀', c, ' ', '('] ++ pretty ty ++ ")"
  pretty (Arrow a b) = left ++ " -> " ++ pretty b
    where
      left = case a of
        Phi c -> [c]
        _ -> "(" ++ pretty a ++ ")"

(-->) :: MLType -> MLType -> MLType
(-->) = Arrow
