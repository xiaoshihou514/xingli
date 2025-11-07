module LCN where

-- Definition for Lambda calculus with names

import LC

data Def = Def
  { name :: String,
    body :: Term
  }

data Program = Program
  { defs :: [Def],
    main :: Term
  }
