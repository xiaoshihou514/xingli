module CurryTypes where

import Data.Char (chr, ord)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import LDef

data CurryType = Phi Char | Arrow CurryType CurryType
  deriving (Show, Eq)

prettyCT :: CurryType -> String
prettyCT (Phi c) = [c]
prettyCT (Arrow a b) = left ++ " -> " ++ prettyCT b
  where
    left = case a of
      Phi c -> [c]
      _ -> "(" ++ prettyCT a ++ ")"

prettyPP :: (TypeCtx, CurryType) -> String
prettyPP (ctx, ty) = "env:\n" ++ show ctx ++ "\n" ++ prettyCT ty

(-->) :: CurryType -> CurryType -> CurryType
(-->) = Arrow

type Label = Char

fresh :: Label -> Label
fresh = chr . (+ 1) . ord

next :: TypeCtx -> (CurryType, TypeCtx)
next (TypeCtx env l) = let l' = fresh l in (Phi l, TypeCtx env l')

add :: Char -> CurryType -> TypeCtx -> TypeCtx
add c ty (TypeCtx env l) = TypeCtx (Map.insert c ty env) l

data TypeCtx = TypeCtx
  { env :: Map Char CurryType,
    label :: Label
  }

instance Show TypeCtx where
  show (TypeCtx env _)
    | null env = "[]"
    | otherwise =
        let show' (v, ty) = [v] ++ ": " ++ prettyCT ty
         in unlines $ map show' $ Map.toList env

instance Eq TypeCtx where
  ctx1 == ctx2 = env ctx1 == env ctx2

emptyEnv :: TypeCtx
emptyEnv = TypeCtx Map.empty 'A'

union :: TypeCtx -> TypeCtx -> TypeCtx
union (TypeCtx envl ll) (TypeCtx envr lr) =
  TypeCtx (Map.union envl envr) (chr (max (ord ll) (ord lr)))

type PrincipalPair = (TypeCtx, CurryType)

-- The pricipal type algorithm for deriving curry types
pp :: Term -> PrincipalPair
pp = pp' emptyEnv
  where
    pp' :: TypeCtx -> Term -> PrincipalPair
    pp' ctx (V c) =
      let (a, ctx') = next ctx
       in (add c a ctx', a)
    pp' ctx (Ab x m) = case Map.lookup x (env ctx') of
      Just ty -> (ctx', ty --> p)
      Nothing -> let (a, ctx'') = next ctx' in (add x a ctx'', a --> p)
      where
        (ctx', p) = pp' ctx m
    pp' ctx (Ap m n) = s2 . liftPP s1 $ (ctx1 `union` ctx3, a)
      where
        (ctx1, p1) = pp' ctx m
        (ctx2, p2) = pp' ctx1 n
        (a, ctx3) = next ctx2
        s1 = unify p1 (p2 --> a)
        s2 = unifyctx (liftM s1 ctx1) (liftM s1 ctx3)

    -- TODO: use fmap
    liftM :: (CurryType -> CurryType) -> (TypeCtx -> TypeCtx)
    liftM f (TypeCtx env l) = TypeCtx (Map.map f env) l

    liftPP :: (CurryType -> CurryType) -> (PrincipalPair -> PrincipalPair)
    liftPP f (TypeCtx env l, a) = (TypeCtx (Map.map f env) l, f a)

    unify :: CurryType -> CurryType -> (CurryType -> CurryType)
    unify left right
      | (Phi p1) <- left, (Phi p2) <- right, p1 == p2 = id
      | (Phi p) <- left,
        p `notOccur` right =
          let subst ty = case ty of
                Phi _ -> if ty == left then right else ty
                Arrow a b -> Arrow (subst a) (subst b)
           in subst
      | (Phi _) <- right = unify right left
      | (Arrow a b) <- left,
        (Arrow c d) <- right =
          let s1 = unify a c
              s2 = unify (s1 b) (s1 d)
           in s2 . s1
      | otherwise = error $ "Cannot unify " ++ prettyCT left ++ " and " ++ prettyCT right
      where
        notOccur :: Label -> CurryType -> Bool
        notOccur p (Phi a) = p /= a
        notOccur p (Arrow a b) = p `notOccur` a && p `notOccur` b

    unifyctx :: TypeCtx -> TypeCtx -> (PrincipalPair -> PrincipalPair)
    unifyctx ctx1 ctx2 = liftPP $ foldr1 (.) subs
      where
        subs = [unify a b | (x, a) <- Map.toList env1, b <- maybeToList $ Map.lookup x env2]
        env1 = env ctx1
        env2 = env ctx2
