module LDef where

data Term = V Char | Ab Char Term | Ap Term Term
  deriving (Eq, Show)

prettyT :: Term -> String
prettyT (V c) = [c]
prettyT (Ab c t) = ('\\' : c : '.' : '(' : prettyT t) ++ ")"
prettyT (Ap f x) = prettyT f ++ (' ' : prettyT x)

omega :: Term
omega = Ap (Ab 'x' (Ap (V 'x') (V 'x'))) (Ab 'x' (Ap (V 'x') (V 'x')))
