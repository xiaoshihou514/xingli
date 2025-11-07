module LC where

import CurryTypes
import Data.Char (chr, ord)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe

-- definition for Lambda calculus

data Term = V Char | Ab Char Term | Ap Term Term
  deriving (Eq, Show)

prettyT :: Term -> String
prettyT (V c) = [c]
prettyT (Ab c t) = ('\\' : c : '.' : '(' : prettyT t) ++ ")"
prettyT (Ap f x) = prettyT f ++ (' ' : prettyT x)

-- Code for deriving Curry type of a Lambda term

type Label = Char

fresh :: Label -> Label
fresh = chr . (+ 1) . ord

next :: TypeCtx CurryType -> (CurryType, TypeCtx CurryType)
next (TypeCtx env l) = let l' = fresh l in (Phi l, TypeCtx env l')

add :: Char -> CurryType -> TypeCtx CurryType -> TypeCtx CurryType
add c ty (TypeCtx env l) = TypeCtx (Map.insert c ty env) l

data TypeCtx a = TypeCtx
  { env :: Map Char a,
    label :: Label
  }

instance Functor TypeCtx where
  fmap f (TypeCtx env l) = TypeCtx (Map.map f env) l

instance Show (TypeCtx CurryType) where
  show (TypeCtx env _)
    | null env = "[]"
    | otherwise =
        let show' (v, ty) = [v] ++ ": " ++ prettyCT ty
         in unlines $ map show' $ Map.toList env

instance Eq (TypeCtx CurryType) where
  ctx1 == ctx2 = env ctx1 == env ctx2

emptyEnv :: TypeCtx CurryType
emptyEnv = TypeCtx Map.empty 'A'

union :: TypeCtx CurryType -> TypeCtx CurryType -> TypeCtx CurryType
union (TypeCtx envl ll) (TypeCtx envr lr) =
  TypeCtx (Map.union envl envr) (chr (max (ord ll) (ord lr)))

type PrincipalPair = (TypeCtx CurryType, CurryType)

prettyPP :: (TypeCtx CurryType, CurryType) -> String
prettyPP (ctx, ty) = "env:\n" ++ show ctx ++ "\n" ++ prettyCT ty

-- The pricipal type algorithm for deriving curry types
pp :: Term -> PrincipalPair
pp = fromJust . pp' emptyEnv
  where
    pp' :: TypeCtx CurryType -> Term -> Maybe PrincipalPair
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
      s2 <- unifyctx (fmap s1 ctx1) (fmap s1 ctx3)
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

    unifyctx :: TypeCtx CurryType -> TypeCtx CurryType -> Maybe (PrincipalPair -> PrincipalPair)
    unifyctx ctx1 ctx2 = liftPP . foldr (.) id <$> sequence subs
      where
        subs :: [Maybe (CurryType -> CurryType)]
        subs = [unify a b | (x, a) <- Map.toList env1, b <- maybeToList $ Map.lookup x env2]
        env1 = env ctx1
        env2 = env ctx2
