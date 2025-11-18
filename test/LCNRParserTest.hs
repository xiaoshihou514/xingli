{-# LANGUAGE OverloadedStrings #-}

module LCNRParserTest where

import Expr.LCNR
import Parser.LCNR ()
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Typeclasses

lcnrParserTests :: TestTree
lcnrParserTests =
  testGroup
    "LCNR Parser Tests"
    [ recursiveDefinitionTests
    ]

recursiveDefinitionTests :: TestTree
recursiveDefinitionTests =
  testGroup
    "Recursive Definition Parsing"
    [ testCase "parse simple recursive definition" $
        parse "rec fact = \\n.(if (isZero n) one (mult n (fact (pred n))))\nfact five"
          --> LCNRProgram
            [RecDef "fact" (Ab 'n' (Ap (Ap (Ap (Name "if") (Ap (Name "isZero") (V 'n'))) (Name "one")) (Ap (Ap (Name "mult") (V 'n')) (Ap (Name "fact") (Ap (Name "pred") (V 'n'))))))]
            (Ap (Name "fact") (Name "five")),
      testCase "parse multiple recursive definitions" $
        parse "rec even = \\n.(if (isZero n) true (odd (pred n)))\nrec odd = \\n.(if (isZero n) false (even (pred n)))\neven four"
          --> LCNRProgram
            [ RecDef "even" (Ab 'n' (Ap (Ap (Ap (Name "if") (Ap (Name "isZero") (V 'n'))) (Name "true")) (Ap (Name "odd") (Ap (Name "pred") (V 'n'))))),
              RecDef "odd" (Ab 'n' (Ap (Ap (Ap (Name "if") (Ap (Name "isZero") (V 'n'))) (Name "false")) (Ap (Name "even") (Ap (Name "pred") (V 'n')))))
            ]
            (Ap (Name "even") (Name "four")),
      testCase "parse mixed recursive and normal definitions" $
        parse "const = \\x.(\\y.(x))\nrec fix = \\f.(f (fix f))\nfix const"
          --> LCNRProgram
            [ Def "const" (Ab 'x' (Ab 'y' (V 'x'))),
              RecDef "fix" (Ab 'f' (Ap (V 'f') (Ap (Name "fix") (V 'f'))))
            ]
            (Ap (Name "fix") (Name "const")),
      testCase "parse recursive definition without main expression" $
        parse "rec omega = (\\x.(x x)) \\x.(x x)\nomega"
          --> LCNRProgram
            [RecDef "omega" (Ap (Ab 'x' (Ap (V 'x') (V 'x'))) (Ab 'x' (Ap (V 'x') (V 'x'))))]
            (Name "omega"),
      testCase "parse recursive definition with complex body" $
        parse "rec map = \\f.(\\l.(if (isNil l) nil (cons (f (head l)) (map f (tail l)))))\nmap"
          --> LCNRProgram
            [RecDef "map" (Ab 'f' (Ab 'l' (Ap (Ap (Ap (Name "if") (Ap (Name "isNil") (V 'l'))) (Name "nil")) (Ap (Ap (Name "cons") (Ap (V 'f') (Ap (Name "head") (V 'l')))) (Ap (Ap (Name "map") (V 'f')) (Ap (Name "tail") (V 'l')))))))]
            (Name "map")
    ]
