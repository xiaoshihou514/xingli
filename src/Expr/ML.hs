module Expr.ML where

import Data.Char (chr, ord)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Typeclasses
import Types.ML

-- Definition for the ML language

data MLTerm where
  V :: String -> MLTerm
  Const :: String -> MLTerm
  Ab :: Char -> MLTerm -> MLTerm
  Ap :: MLTerm -> MLTerm -> MLTerm
  Let :: (String, MLTerm) -> MLTerm -> MLTerm
  Fix :: String -> MLTerm -> MLTerm
  deriving (Eq, Show)

instance Pretty MLTerm where
  pretty :: MLTerm -> String
  pretty (V s) = s
  pretty (Const s) = s
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
  { env :: Map Char MLType,
    tyLabel :: Label,
    varLabel :: Label
  }

emptyCtx :: TypeCtx
emptyCtx = TypeCtx Map.empty 'A' 'a'

union :: TypeCtx -> TypeCtx -> TypeCtx
union (TypeCtx envl ltl lvl) (TypeCtx envr rtl rvl) =
  TypeCtx (Map.union envl envr) (max ltl rtl) (max lvl rvl)

nextTy :: TypeCtx -> (MLType, TypeCtx)
nextTy (TypeCtx env tl vl) = let tl' = fresh tl in (Phi tl, TypeCtx env tl' vl)

nextVar :: TypeCtx -> (MLType, TypeCtx)
nextVar (TypeCtx env tl vl) = let vl' = fresh vl in (Forall tl, TypeCtx env tl vl')

add :: Char -> MLType -> TypeCtx -> TypeCtx
add c ty (TypeCtx env tl vl) = TypeCtx (Map.insert c ty env) tl vl

apply :: (MLType -> MLType) -> TypeCtx -> TypeCtx
apply f (TypeCtx env tl vl) = TypeCtx (Map.map f env) tl vl

instance Pretty TypeCtx where
  pretty (TypeCtx env _ _)
    | null env = "[]"
    | otherwise =
        let show' (v, ty) = [v] ++ ": " ++ pretty ty
         in unlines $ map show' $ Map.toList env

instance Show TypeCtx where -- needed for tests
  show = pretty

type PrincipalPair = (MLType -> MLType, MLType)

type Env = Map String MLType

algorithmW :: Env -> MLTerm -> Maybe MLType
algorithmW e t = snd . snd <$> algorithmW' emptyCtx e t
  where
    algorithmW' :: TypeCtx -> Env -> MLTerm -> Maybe (TypeCtx, PrincipalPair)
    algorithmW' ctx v (Const s) = do
      a <- v !? s
      let (ctx', b) = freshInstance (ctx, a)
      return (ctx', (id, b))
    algorithmW' _ _ _ = undefined

    freshInstance :: (TypeCtx, MLType) -> (TypeCtx, MLType)
    freshInstance = undefined
