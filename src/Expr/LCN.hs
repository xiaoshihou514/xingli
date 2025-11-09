module Expr.LCN where

import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Expr.LC hiding (Ab, Ap, V)
import Pretty
import Types.CurryTypes

-- Definition for Lambda calculus with names

data LCTerm = V Char | Ab Char LCTerm | Ap LCTerm LCTerm | Name String
  deriving (Eq, Show)

instance Pretty LCTerm where
  pretty :: LCTerm -> String
  pretty (V c) = [c]
  pretty (Ab c t) = ('\\' : c : '.' : '(' : pretty t) ++ ")"
  pretty (Ap f x) = pretty f ++ (' ' : pretty x)
  pretty (Name s) = s

data Def = Def
  { name :: String,
    body :: LCTerm
  }

data Program = Program
  { defs :: [Def],
    main :: LCTerm
  }

-- Definition for Env
type Env = Map String CurryType

-- Principal pair algorithm (for Lambda calculus with names)
ppln :: Program -> Maybe PrincipalPair
ppln (Program defs main) = do
  env <- buildEnv defs
  ppln' main env emptyEnv
  where
    buildEnv :: [Def] -> Maybe Env
    buildEnv = foldr buildEnv' (Just Map.empty)
      where
        buildEnv' :: Def -> Maybe Env -> Maybe Env
        buildEnv' (Def name m) env = do
          (_, a) <- ppln' m Map.empty emptyEnv
          Map.insert name a <$> env

    ppln' :: LCTerm -> Env -> TypeCtx -> Maybe PrincipalPair
    -- The only added case
    ppln' (Name n) e ctx = do
      ty <- e !? n
      return $ freshInstance ctx ty
    -- The following are exactly the same except we pass Env around
    ppln' (V c) _ ctx =
      let (a, ctx') = next ctx
       in Just (add c a ctx', a)
    ppln' (Ab x m) e ctx = do
      (ctx', p) <- ppln' m e ctx
      case env ctx' !? x of
        Just ty -> Just (ctx', ty --> p)
        Nothing -> do
          let (a, ctx'') = next ctx'
          return (add x a ctx'', a --> p)
    ppln' (Ap m n) e ctx = do
      (ctx1, p1) <- ppln' m e ctx
      (ctx2, p2) <- ppln' n e ctx1
      let (a, ctx3) = next ctx2
      s1 <- unify p1 (p2 --> a)
      s2 <- unifyctx (apply s1 ctx1) (apply s1 ctx3)
      return $ s2 . liftPP s1 $ (ctx1 `union` ctx3, a)

    freshInstance :: TypeCtx -> CurryType -> PrincipalPair
    freshInstance = (fst .) . freshInstance' Map.empty
      where
        freshInstance' ::
          Map Char CurryType ->
          TypeCtx ->
          CurryType ->
          (PrincipalPair, Map Char CurryType)
        freshInstance' env ctx (Phi c) = case env !? c of
          Just ty -> ((ctx, ty), env)
          Nothing -> let (ty', ctx') = next ctx in ((ctx', ty'), Map.insert c ty' env)
        freshInstance' env ctx (Arrow left right) = ((ctx'', Arrow tyl tyr), env'')
          where
            ((ctx', tyl), env') = freshInstance' env ctx left
            ((ctx'', tyr), env'') = freshInstance' env' ctx' right
