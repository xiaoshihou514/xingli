module Expr.ML where

import Data.Char (chr, ord)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Typeclasses
import Types.ML

-- Definition for the ML language

data MLTerm where
  V :: String -> MLTerm
  Const :: String -> MLTerm
  Ab :: String -> MLTerm -> MLTerm
  Ap :: MLTerm -> MLTerm -> MLTerm
  Let :: (String, MLTerm) -> MLTerm -> MLTerm
  Fix :: String -> MLTerm -> MLTerm
  deriving (Eq, Show)

instance Pretty MLTerm where
  pretty :: MLTerm -> String
  pretty (V s) = s
  pretty (Const s) = s
  pretty (Ab c t) = ("\\" ++ c ++ '.' : '(' : pretty t) ++ ")"
  pretty (Ap f x) = pretty f ++ (' ' : pretty x)
  pretty (Let (x, e1) e2) =
    "let " ++ x ++ " = " ++ pretty e1 ++ " in " ++ pretty e2
  pretty (Fix name e) = "fix " ++ name ++ "." ++ pretty e

-- Code for Algorithm W

-- The label state for making fresh type labels
type Label = Char

fresh :: Label -> Label
fresh = chr . (+ 1) . ord

data TypeCtx = TypeCtx
  { env :: Map String MLType,
    tyLabel :: Label
  }

emptyCtx :: TypeCtx
emptyCtx = TypeCtx Map.empty 'a'

union :: TypeCtx -> TypeCtx -> TypeCtx
union (TypeCtx envl ll) (TypeCtx envr lr) =
  TypeCtx (Map.union envl envr) (max ll lr)

next :: TypeCtx -> (MLType, TypeCtx)
next (TypeCtx env l) = let l' = fresh l in (Phi l, TypeCtx env l')

add :: String -> MLType -> TypeCtx -> TypeCtx
add c ty (TypeCtx env l) = TypeCtx (Map.insert c ty env) l

apply :: (MLType -> MLType) -> TypeCtx -> TypeCtx
apply f (TypeCtx env l) = TypeCtx (Map.map f env) l

instance Pretty TypeCtx where
  pretty (TypeCtx env _)
    | null env = "[]"
    | otherwise =
        let show' (v, ty) = v ++ ": " ++ pretty ty
         in unlines $ map show' $ Map.toList env

instance Show TypeCtx where -- needed for tests
  show = pretty

type PrincipalPair = (MLType -> MLType, MLType)

type Env = Map String MLType

algorithmW :: Env -> MLTerm -> Maybe MLType
algorithmW v term_ = snd . snd <$> algorithmW' emptyCtx term_
  where
    algorithmW' :: TypeCtx -> MLTerm -> Maybe (TypeCtx, PrincipalPair)
    algorithmW' ctx (Const s) = do
      a <- v !? s
      let (ctx', b) = freshInstance (ctx, a)
      return (ctx', (id, b))
    algorithmW' ctx (V c) = do
      a <- env ctx !? c
      let (ctx', b) = freshInstance (ctx, a)
      return (ctx', (id, b))
    algorithmW' ctx (Ab x e) = do
      let (phi, ctx') = next ctx
      (ctx'', (s, a)) <- algorithmW' (add x phi ctx') e
      return (ctx'', (s, s (phi --> a)))
    algorithmW' ctx (Let (x, e1) e2) = do
      (TypeCtx _ l, (s1, a)) <- algorithmW' ctx e1
      let (TypeCtx mapping _) = apply s1 ctx
      let ctx'' = TypeCtx mapping l
      let sigma = generalize ctx'' a
      (ctx''', (s2, b)) <- algorithmW' (add x sigma ctx'') e2
      return (ctx''', (s2 . s1, b))
    algorithmW' ctx (Fix g e) = do
      let (phi, ctx') = next ctx
      (ctx'', (s1, a)) <- algorithmW' (add g phi ctx') e
      s2 <- unify (s1 phi) a
      return (ctx'', (s2 . s1, s2 a))
    algorithmW' ctx (Ap e1 e2) = do
      (ctx', (s1, a)) <- algorithmW' ctx e1
      (ctx'', (s2, b)) <- algorithmW' (apply s1 ctx') e2
      let (phi, ctx''') = next ctx''
      s3 <- unify (s2 a) (b --> phi)
      return (ctx''', (s3 . s2 . s1, s3 phi))

    -- In Expr.ML.hs
    freshInstance :: (TypeCtx, MLType) -> (TypeCtx, MLType)
    freshInstance = snd . freshInstance' Map.empty
      where
        freshInstance' ::
          Map Char MLType ->
          (TypeCtx, MLType) ->
          (Map Char MLType, (TypeCtx, MLType))
        freshInstance' env (ctx, Qtf c ty) =
          let (freshTy, ctx') = next ctx
              env' = Map.insert c freshTy env
           in freshInstance' env' (ctx', ty)
        freshInstance' env (ctx, Phi c) =
          case Map.lookup c env of
            Just ty -> (env, (ctx, ty))
            -- Free variable, no substitution
            Nothing -> (env, (ctx, Phi c))
        freshInstance' env (ctx, Arrow left right) =
          let (env', (ctx', left')) = freshInstance' env (ctx, left)
              (env'', (ctx'', right')) = freshInstance' env' (ctx', right)
           in (env'', (ctx'', Arrow left' right'))
        freshInstance' env x = (env, x)

    -- Quantify all free type vars in ty that are quantified in ctx
    generalize :: TypeCtx -> MLType -> MLType
    -- let's gooooo
    generalize ctx ty = Set.foldl (\f -> (f .) . Qtf) id (fvty \\ fvctx) ty
      where
        fvctx :: Set Char
        fvctx = Set.unions $ map (free . snd) $ Map.toList (env ctx)

        fvty :: Set Char
        fvty = free ty

        free :: MLType -> Set Char
        free = free' Set.empty
          where
            free' :: Set Char -> MLType -> Set Char
            -- top level
            free' env (Qtf c t) = free' (Set.insert c env) t
            free' env (Phi c)
              | c `elem` env = Set.empty
              | otherwise = Set.fromList [c]
            free' env (Arrow l r) = Set.union (free' env l) (free' env r)
            free' _ (Basic _) = Set.empty

    unify :: MLType -> MLType -> Maybe (MLType -> MLType)
    unify left right
      -- same as in LC*
      | (Phi p1) <- left, (Phi p2) <- right, p1 == p2 = Just id
      -- merged with ML ty unify
      | (Phi p) <- left,
        p `notOccur` right =
          let subst ty = case ty of
                Phi _ -> if ty == left then right else ty
                Arrow a b -> Arrow (subst a) (subst b)
                Qtf c ty' -> if p /= c then Qtf c (subst ty') else ty
                _ -> ty
           in Just subst
      | (Arrow a b) <- left,
        (Arrow c d) <- right =
          do
            s1 <- unify a c
            s2 <- unify (s1 b) (s1 d)
            return $ s2 . s1
      -- added case
      | (Basic c1) <- left, (Basic c2) <- right, c1 == c2 = Just id
      -- merged with ML ty unify
      | (Phi _) <- right = unify right left
      | otherwise = Nothing
      where
        notOccur :: Label -> MLType -> Bool
        notOccur p (Phi a) = p /= a
        notOccur _ (Basic _) = True
        notOccur p (Qtf _ a) = p `notOccur` a
        notOccur p (Arrow a b) = p `notOccur` a && p `notOccur` b
