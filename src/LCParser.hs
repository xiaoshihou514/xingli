module LCParser where

-- Parser for lambda calculus

import Expr.LC

type Parser a = String -> (a, String)

parse :: String -> Term
parse input = case parse' id input of
  (t, "") -> t
  _ -> error "Broken input"
  where
    parse' :: (Term -> Term) -> Parser Term
    parse' f ('\\' : c : '.' : '(' : cs) = case cs' of
      ' ' : cs'' ->
        let (t'', cs''') = parse' (Ap t') cs''
         in (t'', cs''')
      _ -> (t', cs')
      where
        (t, cs') = case parse' id cs of
          (t'', ')' : cs'') -> (t'', cs'')
          _ -> error "Unclosed paren"
        t' = f (Ab c t)
    parse' f ('(' : cs) = case cs' of
      ' ' : cs'' ->
        let (t'', cs''') = parse' (Ap t') cs''
         in (t'', cs''')
      _ -> (t', cs')
      where
        (t, cs') = case parse' id cs of
          (t'', ')' : cs'') -> (t'', cs'')
          _ -> error "Unclosed paren"
        t' = f t
    parse' f (c : cs) = case cs of
      ' ' : cs' ->
        let (t', cs'') = parse' (Ap v) cs'
         in (t', cs'')
      _ -> (v, cs)
      where
        v = f (V c)
    parse' _ "" = error "Incomplete input"
