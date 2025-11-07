module CurryTypes where

-- Definition for the Curry type system

data CurryType = Phi Char | Arrow CurryType CurryType
  deriving (Show, Eq)

prettyCT :: CurryType -> String
prettyCT (Phi c) = [c]
prettyCT (Arrow a b) = left ++ " -> " ++ prettyCT b
  where
    left = case a of
      Phi c -> [c]
      _ -> "(" ++ prettyCT a ++ ")"

(-->) :: CurryType -> CurryType -> CurryType
(-->) = Arrow
