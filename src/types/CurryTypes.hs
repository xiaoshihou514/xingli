module CurryTypes where

import Pretty

-- Definition for the Curry type system

data CurryType = Phi Char | Arrow CurryType CurryType
  deriving (Show, Eq)

instance Pretty CurryType where
  pretty :: CurryType -> String
  pretty (Phi c) = [c]
  pretty (Arrow a b) = left ++ " -> " ++ pretty b
    where
      left = case a of
        Phi c -> [c]
        _ -> "(" ++ pretty a ++ ")"

(-->) :: CurryType -> CurryType -> CurryType
(-->) = Arrow
