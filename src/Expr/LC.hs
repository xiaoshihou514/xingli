module Expr.LC where

import Data.Char (chr, ord)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Pretty
import Types.CurryTypes

-- definition for Lambda calculus

data Term = V Char | Ab Char Term | Ap Term Term
  deriving (Eq, Show)

instance Pretty Term where
  pretty :: Term -> String
  pretty (V c) = [c]
  pretty (Ab c t) = ('\\' : c : '.' : '(' : pretty t) ++ ")"
  pretty (Ap f x) = pretty f ++ (' ' : pretty x)

-- Code for deriving Curry type of a Lambda term

-- The label state for making fresh type labels
type Label = Char

fresh :: Label -> Label
fresh = chr . (+ 1) . ord

-- The context for the principal pair algorithm
data TypeCtx = TypeCtx
  { env :: Map Char CurryType,
    label :: Label
  }

emptyEnv :: TypeCtx
emptyEnv = TypeCtx Map.empty 'A'

union :: TypeCtx -> TypeCtx -> TypeCtx
union (TypeCtx envl ll) (TypeCtx envr lr) =
  TypeCtx (Map.union envl envr) (chr (max (ord ll) (ord lr)))

next :: TypeCtx -> (CurryType, TypeCtx)
next (TypeCtx env l) = let l' = fresh l in (Phi l, TypeCtx env l')

add :: Char -> CurryType -> TypeCtx -> TypeCtx
add c ty (TypeCtx env l) = TypeCtx (Map.insert c ty env) l

apply :: (CurryType -> CurryType) -> TypeCtx -> TypeCtx
apply f (TypeCtx env l) = TypeCtx (Map.map f env) l

instance Pretty TypeCtx where
  pretty (TypeCtx env _)
    | null env = "[]"
    | otherwise =
        let show' (v, ty) = [v] ++ ": " ++ pretty ty
         in unlines $ map show' $ Map.toList env

instance Show TypeCtx where -- needed for tests
  show = pretty

instance Eq TypeCtx where -- ignore label state
  ctx1 == ctx2 = env ctx1 == env ctx2

type PrincipalPair = (TypeCtx, CurryType)

instance Pretty PrincipalPair where
  pretty :: (TypeCtx, CurryType) -> String
  pretty (ctx, ty) = "env:\n" ++ pretty ctx ++ "\n" ++ pretty ty

-- The pricipal type algorithm for deriving curry types
pp :: Term -> PrincipalPair
pp = fromJust . pp' emptyEnv
  where
    pp' :: TypeCtx -> Term -> Maybe PrincipalPair
    pp' ctx (V c) =
      let (a, ctx') = next ctx
       in Just (add c a ctx', a)
    pp' ctx (Ab x m) = do
      (ctx', p) <- pp' ctx m
      case Map.lookup x (env ctx') of
        Just ty -> Just (ctx', ty --> p)
        Nothing -> do
          let (a, ctx'') = next ctx'
          return (add x a ctx'', a --> p)
    pp' ctx (Ap m n) = do
      (ctx1, p1) <- pp' ctx m
      (ctx2, p2) <- pp' ctx1 n
      let (a, ctx3) = next ctx2
      s1 <- unify p1 (p2 --> a)
      s2 <- unifyctx (apply s1 ctx1) (apply s1 ctx3)
      return $ s2 . liftPP s1 $ (ctx1 `union` ctx3, a)

    liftPP :: (CurryType -> CurryType) -> (PrincipalPair -> PrincipalPair)
    liftPP f (TypeCtx env l, a) = (TypeCtx (Map.map f env) l, f a)

    unify :: CurryType -> CurryType -> Maybe (CurryType -> CurryType)
    unify left right
      | (Phi p1) <- left, (Phi p2) <- right, p1 == p2 = Just id
      | (Phi p) <- left,
        p `notOccur` right =
          let subst ty = case ty of
                Phi _ -> if ty == left then right else ty
                Arrow a b -> Arrow (subst a) (subst b)
           in Just subst
      | (Phi _) <- right = unify right left
      | (Arrow a b) <- left,
        (Arrow c d) <- right =
          do
            s1 <- unify a c
            s2 <- unify (s1 b) (s1 d)
            return $ s2 . s1
      | otherwise = Nothing
      where
        notOccur :: Label -> CurryType -> Bool
        notOccur p (Phi a) = p /= a
        notOccur p (Arrow a b) = p `notOccur` a && p `notOccur` b

    unifyctx :: TypeCtx -> TypeCtx -> Maybe (PrincipalPair -> PrincipalPair)
    unifyctx ctx1 ctx2 = liftPP . foldr (.) id <$> sequence subs
      where
        subs :: [Maybe (CurryType -> CurryType)]
        subs = [unify a b | (x, a) <- Map.toList env1, b <- maybeToList $ Map.lookup x env2]
        env1 = env ctx1
        env2 = env ctx2
