-- kill me
{-# OPTIONS_GHC -Wno-orphans #-}

module Parser.ML where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Expr.ML
import Typeclasses

takeEnd :: Int -> [a] -> [a]
takeEnd 0 _ = []
takeEnd n xsy =
  let (xs, y) = fromJust $ unsnoc xsy
      ys = takeEnd (n - 1) xs
   in ys ++ [y]

dropEnd :: Int -> [a] -> [a]
dropEnd 0 xs = xs
dropEnd n xsx =
  let (xs, _) = fromJust $ unsnoc xsx
   in dropEnd (n - 1) xs

-- Parser for lambda calculus
type Parser a = String -> (a, String)

instance FromPretty MLTerm where
  parse :: String -> MLTerm
  parse input = foldl (\acc -> (acc .) . Let) id defs main
    where
      (lets, mainln) = fromJust $ unsnoc $ lines input
      defs = map parseN lets
      main = fst $ parseML mainln

      parseN :: String -> (String, MLTerm)
      parseN line = fromJust $ do
        guard (take letlen line /= "let ")
        guard (takeEnd inlen line /= " in")
        let line' = dropEnd inlen $ drop letlen line
        let (name, rest) = break (== '=') line'
        let (e, empty) = parseML (drop eqlen rest)
        guard (not $ null empty)
        return (init name, e)

      letlen = length "let "
      inlen = length " in"
      eqlen = length "= "

parseML :: Parser MLTerm
parseML = parse' id []
  where
    parse' :: (MLTerm -> MLTerm) -> String -> Parser MLTerm
    parse' f bv ('\\' : c : '.' : cs) =
      let (t, cs') = parse' id (c : bv) cs
       in (f (Ab [c] t), cs')
    parse' f bv ('f' : 'i' : 'x' : ' ' : c : '.' : cs) =
      let (t, cs') = parse' id (c : bv) cs
       in (f (Fix [c] t), cs')
    parse' f bv ('(' : cs) =
      case parse' id bv cs of
        (t, ')' : cs') ->
          case cs' of
            ' ' : cs'' ->
              let (t', cs''') = parse' (Ap t) bv cs''
               in (t', cs''')
            _ -> (f t, cs')
        _ -> error "Unclosed paren"
    parse' f bv cs =
      case span isAlpha cs of
        (name, rest) ->
          case rest of
            ' ' : cs' ->
              let v = if name `isInfixOf` bv then V name else Const name
                  (t', cs'') = parse' (Ap (f v)) bv cs'
               in (t', cs'')
            _ ->
              let term =
                    if name `isInfixOf` bv
                      then V name
                      else Const name
               in (f term, rest)
