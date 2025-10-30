module LDef where

data Term = V Char | Ab Char Term | Ap Term Term
  deriving (Eq, Show)

pretty :: Term -> String
pretty (V c) = [c]
pretty (Ab c t) = ('\\' : c : '.' : '(' : pretty t) ++ ")"
pretty (Ap f x) = pretty f ++ (' ' : pretty x)

omega :: Term
omega = Ap (Ab 'x' (Ap (V 'x') (V 'x'))) (Ab 'x' (Ap (V 'x') (V 'x')))
