module Expr.HL where

-- Definition for the ML language

data MLTerm
  = V String
  | IntLit Int
  | BoolLit Bool
  | Ab Char MLTerm
  | Ap MLTerm MLTerm
  | Let (String, MLTerm) MLTerm
  | Fix String MLTerm
  deriving (Eq, Show)

instance Pretty MLTerm where
  pretty :: MLTerm -> String
  pretty (V s) = s
  pretty (IntLit i) = show i
  pretty (BoolLit b) = show b
  pretty (Ab c t) = ('\\' : c : '.' : '(' : pretty t) ++ ")"
  pretty (Ap f x) = pretty f ++ (' ' : pretty x)
  pretty (Let (x, e1) e2) = "let " ++ x ++ " = " ++ pretty e1 ++ " in " ++ pretty e2
  pretty (Fix name e) = "fix " ++ name ++ "." ++ pretty e

-- Code for Algorithm W

-- The label state for making fresh type labels
type Label = Char

fresh :: Label -> Label
fresh = chr . (+ 1) . ord

data TypeCtx = TypeCtx
  { env :: Map Char CurryType,
    tyLabel :: Label,
    varLabel :: Label
  }
