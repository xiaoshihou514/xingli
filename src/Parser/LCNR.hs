-- kill me
{-# OPTIONS_GHC -Wno-orphans #-}

module Parser.LCNR where

import Data.Char
import Data.List
import Data.Maybe
import Expr.LCNR
import Typeclasses

-- Parser for lambda calculus
type Parser a = String -> (a, String)

instance FromPretty LCNRProgram where
  parse :: String -> LCNRProgram
  parse input = LCNRProgram defs main
    where
      (deflns, mainln) = fromJust $ unsnoc $ lines input
      defs = map parseN deflns
      main = parseLCNR mainln

      parseN :: String -> Def
      parseN line =
        let (name', rest) = break (== '=') line
            (def, name) = case init name' of
              'r' : 'e' : 'c' : ' ' : cs -> (RecDef, cs)
              cs -> (Def, cs)
            body = parseLCNR $ drop 2 rest
         in def name body

parseLCNR :: String -> LCNRTerm
parseLCNR input = case parse' id input of
  (t, "") -> t
  _ -> error "Broken input"
  where
    parse' :: (LCNRTerm -> LCNRTerm) -> Parser LCNRTerm
    parse' f ('\\' : c : '.' : '(' : cs) = case cs' of
      ' ' : cs'' ->
        let (t'', cs''') = parse' (Ap t') cs''
         in (t'', cs''')
      _ -> (t', cs')
      where
        (t, cs') = case parse' id cs of
          (t'', ')' : cs'') -> (t'', cs'')
          _ -> error "Unclosed paren: "
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
    parse' f cs = case cs' of
      ' ' : cs'' ->
        let (t', cs''') = parse' (Ap v) cs''
         in (t', cs''')
      _ -> (v, cs')
      where
        (name, cs') = span isAlpha cs
        v = case name of
          [c] -> f (V c)
          _ -> f (Name name)
