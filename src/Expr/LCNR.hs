module Expr.LCNR (LCNRTerm (..), Def (..), LCNRProgram (..), TypeCtx (..), ppln) where

import Data.Bifunctor (first)
import Data.Char
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe
import Typeclasses
import Types.CurryTypes

-- Definition for Lambda calculus with names and recursion

data LCNRTerm
  = V Char
  | Ab Char LCNRTerm
  | Ap LCNRTerm LCNRTerm
  | Name String
  deriving (Eq, Show)

instance Pretty LCNRTerm where
  pretty :: LCNRTerm -> String
  pretty (V c) = [c]
  pretty (Ab c t) = ('\\' : c : '.' : '(' : pretty t) ++ ")"
  pretty (Ap f x) = pretty f ++ (' ' : pretty x)
  pretty (Name s) = s

data Def
  = Def
      { name :: String,
        body :: LCNRTerm
      }
  | -- Added case
    RecDef
      { name :: String,
        body :: LCNRTerm
      }
  deriving (Eq, Show)

data LCNRProgram = LCNRProgram
  { defs :: [Def],
    main :: LCNRTerm
  }
  deriving (Eq, Show)

-- Code for deriving Curry type of a LCNR term

-- The label state for making fresh type labels
type Label = Char

fresh :: Label -> Label
fresh = chr . (+ 1) . ord

-- Definition for Env
data TypeCtx = TypeCtx
  { env :: Map Char CurryType,
    label :: Label
  }

emptyCtx :: TypeCtx
emptyCtx = TypeCtx Map.empty 'A'

union :: TypeCtx -> TypeCtx -> TypeCtx
union (TypeCtx envl ll) (TypeCtx envr lr) =
  TypeCtx (Map.union envl envr) (max ll lr)

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

-- Added field for is rec def
type Env = Map String (CurryType, Bool)

emptyEnv :: Env
emptyEnv = Map.empty

addEnv :: String -> CurryType -> Env -> Env
addEnv x ty = Map.insert x (ty, False)

addEnvRec :: String -> CurryType -> Env -> Env
addEnvRec x ty = Map.insert x (ty, True)

-- instance Pretty Env where
--   pretty = unlines . map pretty' . Map.toList
--     where
--       pretty' (name, (a, rec)) =
--         ( if rec
--             then "REC "
--             else ""
--         )
--           ++ name
--           ++ ": "
--           ++ pretty a

type PrincipalTriple = (TypeCtx, CurryType, Env)

-- Principal pair algorithm (for Lambda calculus with names)
ppln :: LCNRProgram -> Maybe PrincipalTriple
ppln (LCNRProgram defs main) = do
  let context = emptyCtx
  (context', env) <- buildEnv context defs
  ppln' main env context'
  where
    buildEnv :: TypeCtx -> [Def] -> Maybe (TypeCtx, Env)
    buildEnv c = foldl (flip buildEnv') $ Just (c, emptyEnv)
      where
        buildEnv' :: Def -> Maybe (TypeCtx, Env) -> Maybe (TypeCtx, Env)
        buildEnv' (Def name m) macc = do
          (ctx, env) <- macc
          -- use env instead of empty
          (ctx', a, env') <- ppln' m env ctx
          -- non rec case
          return (ctx', addEnv name a env')
        buildEnv' (RecDef name m) macc = do
          (ctx, env) <- macc
          -- infer with new type phi
          let (phi, ctx') = next ctx
          (ctx'', a, env') <- ppln' m (addEnvRec name phi env) ctx'
          -- retrieve result from env
          (b, True) <- env' !? name
          s <- unify a b
          -- rec case
          return (ctx'', addEnvRec name (s a) env)

    ppln' :: LCNRTerm -> Env -> TypeCtx -> Maybe PrincipalTriple
    -- The only added case
    ppln' (Name n) e ctx = do
      (ty, rec) <- e !? n
      return $
        if rec
          then (ctx, ty, e)
          else freshInstance e ctx ty

    -- The following are exactly the same except we pass Env around
    ppln' (V c) e ctx =
      let (a, ctx') = next ctx
       in Just (add c a ctx', a, e)
    ppln' (Ab x m) e ctx = do
      (ctx', p, e') <- ppln' m e ctx
      case env ctx' !? x of
        Just ty -> Just (ctx', ty --> p, e')
        Nothing -> do
          let (a, ctx'') = next ctx'
          return (add x a ctx'', a --> p, e')
    ppln' (Ap m n) e ctx = do
      (ctx1, p1, e') <- ppln' m e ctx
      (ctx2, p2, e'') <- ppln' n e' ctx1
      let (a, ctx3) = next ctx2
      s1 <- unify p1 (p2 --> a)
      s2 <- unifyctx (apply s1 ctx1) (apply s1 ctx3)
      return $ s2 . liftPP s1 $ (ctx1 `union` ctx3, a, e'')

    freshInstance :: Env -> TypeCtx -> CurryType -> PrincipalTriple
    freshInstance env context phi = fst $ freshInstance' Map.empty env context phi
      where
        freshInstance' ::
          Map Char CurryType ->
          Env ->
          TypeCtx ->
          CurryType ->
          (PrincipalTriple, Map Char CurryType)
        freshInstance' memo e ctx (Phi c) = case memo !? c of
          Just ty -> ((ctx, ty, e), memo)
          Nothing -> let (ty', ctx') = next ctx in ((ctx', ty', e), Map.insert c ty' memo)
        freshInstance' memo e ctx (Arrow left right) = ((ctx'', Arrow tyl tyr, e), memo'')
          where
            ((ctx', tyl, _), memo') = freshInstance' memo e ctx left
            ((ctx'', tyr, _), memo'') = freshInstance' memo' e ctx' right

    liftPP :: (CurryType -> CurryType) -> (PrincipalTriple -> PrincipalTriple)
    liftPP f (t, a, e) = (apply f t, f a, Map.map (first f) e)

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

    unifyctx :: TypeCtx -> TypeCtx -> Maybe (PrincipalTriple -> PrincipalTriple)
    unifyctx ctx1 ctx2 = liftPP . foldr (.) id <$> sequence subs
      where
        subs :: [Maybe (CurryType -> CurryType)]
        subs = [unify a b | (x, a) <- Map.toList env1, b <- maybeToList $ Map.lookup x env2]
        env1 = env ctx1
        env2 = env ctx2
