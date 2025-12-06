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
  parse input =
    let lines' = filter (not . null) $ map (dropWhile isSpace) $ lines input
     in case lines' of
          [] -> error "Empty input"
          _ ->
            let (lets, mainln) = fromJust $ unsnoc lines'
                -- Parse let bindings in order, accumulating bound names
                (defs, boundNames) = foldl parseOne ([], []) lets
                main = fst $ parseML boundNames mainln
             in foldl (\acc (name, term) -> Let (name, term) acc) main defs
    where
      parseOne :: ([(String, MLTerm)], [String]) -> String -> ([(String, MLTerm)], [String])
      parseOne (defs, bv) line =
        fromJust $ do
          guard (take letlen line == "let ")
          guard (takeEnd inlen line == " in")
          let line' = dropEnd inlen $ drop letlen line
          let (name, rest) = break (== '=') line'
          guard (not $ null name)
          let varName = takeWhile (not . isSpace) $ dropWhile isSpace $ init name
          let (e, empty) = parseML bv (drop eqlen rest)
          guard (null empty)
          return ((varName, e) : defs, varName : bv)

      letlen = length "let "
      inlen = length " in"
      eqlen = length "= "

-- Deepseek generated
-- Parse with bound variable context
parseML :: [String] -> Parser MLTerm
parseML bv_ cs_ =
  let (term, rest) = parseTerm bv_ cs_
   in continueApp bv_ term rest
  where
    continueApp :: [String] -> MLTerm -> String -> (MLTerm, String)
    continueApp bv term rest =
      case dropWhile isSpace rest of
        "" -> (term, "")
        (c : cs')
          | canStartTerm c ->
              let (nextTerm, rest') = parseTerm bv (c : cs')
               in continueApp bv (Ap term nextTerm) rest'
        _ -> (term, rest)

    canStartTerm :: Char -> Bool
    canStartTerm c = isAlpha c || c == '\\' || c == '(' || c == 'f'

    parseTerm :: [String] -> Parser MLTerm
    parseTerm _ [] = error "Unexpected end of input"
    parseTerm bv cs = case dropWhile isSpace cs of
      -- Abstraction: \x. body
      ('\\' : cs1) ->
        let (var, rest) = span isAlpha cs1
         in if not (null var)
              then case dropWhile isSpace rest of
                ('.' : rest') ->
                  let (t, cs') = parseML (var : bv) rest'
                   in (Ab var t, cs')
                _ -> error $ "Expected '.' after variable in abstraction, got: " ++ rest
              else error "Abstraction variable must be non-empty"
      -- Fixpoint: fix x. body
      ('f' : 'i' : 'x' : cs1) ->
        case dropWhile isSpace cs1 of
          (c : cs2)
            | isAlpha c ->
                let (var, rest) = span isAlpha (c : cs2)
                 in if not (null var)
                      then case dropWhile isSpace rest of
                        ('.' : rest') ->
                          let (t, cs') = parseML (var : bv) rest'
                           in (Fix var t, cs')
                        _ -> error "Expected '.' after variable in fixpoint"
                      else error "Fixpoint variable must be non-empty"
          _ -> error "Expected variable after 'fix'"
      -- Parenthesized expression
      ('(' : cs1) ->
        let (t, cs2) = parseML bv cs1
         in case dropWhile isSpace cs2 of
              (')' : cs3) -> (t, cs3)
              _ -> error $ "Unclosed parenthesis, remaining: " ++ cs2
      -- Variable or Constant
      _ ->
        let (name, cs') = span (\c -> isAlphaNum c || c == '_') cs
         in case name of
              "" -> error "Empty identifier"
              s ->
                -- Single character is always V, multi-character depends on context
                if length s == 1
                  then (V s, cs')
                  else
                    let term = if s `elem` bv then V s else Const s
                     in (term, cs')
