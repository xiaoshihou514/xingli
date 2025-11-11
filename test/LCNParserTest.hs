{-# LANGUAGE OverloadedStrings #-}

module LCNParserTest where

import Expr.LCN
import Parser.LCN (parseLCN)
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Typeclasses

lcnParserTests :: TestTree
lcnParserTests =
  testGroup
    "LCN Parser Tests"
    [ variableTests,
      abstractionTests,
      applicationTests,
      nameTests,
      programTests,
      complexTests
    ]

variableTests :: TestTree
variableTests =
  testGroup
    "Variable Parsing"
    [ testCase "parse single character variable" $
        parseLCN "x" --> V 'x',
      testCase "parse another variable" $
        parseLCN "a" --> V 'a'
    ]

abstractionTests :: TestTree
abstractionTests =
  testGroup
    "Abstraction Parsing"
    [ testCase "parse simple abstraction" $
        parseLCN "\\x.(x)" --> Ab 'x' (V 'x'),
      testCase "parse nested abstraction" $
        parseLCN "\\x.(\\y.(x))" --> Ab 'x' (Ab 'y' (V 'x')),
      testCase "parse abstraction with application inside" $
        parseLCN "\\x.(x x)" --> Ab 'x' (Ap (V 'x') (V 'x')),
      testCase "parse abstraction with named term" $
        parseLCN "\\x.(identity)" --> Ab 'x' (Name "identity")
    ]

applicationTests :: TestTree
applicationTests =
  testGroup
    "Application Parsing"
    [ testCase "parse simple application" $
        parseLCN "x y" --> Ap (V 'x') (V 'y'),
      testCase "parse left-associative application" $
        parseLCN "x y z" --> Ap (Ap (V 'x') (V 'y')) (V 'z'),
      testCase "parse application with abstraction" $
        parseLCN "\\x.(x) y" --> Ap (Ab 'x' (V 'x')) (V 'y'),
      testCase "parse application with named terms" $
        parseLCN "f x" --> Ap (V 'f') (V 'x'),
      testCase "parse mixed application" $
        parseLCN "map f list" --> Ap (Ap (Name "map") (V 'f')) (Name "list")
    ]

nameTests :: TestTree
nameTests =
  testGroup
    "Name Parsing"
    [ testCase "parse simple name" $
        parseLCN "identity" --> Name "identity",
      testCase "parse multi-character name" $
        parseLCN "mapFunction" --> Name "mapFunction",
      testCase "parse name starting with capital" $
        parseLCN "Identity" --> Name "Identity",
      testCase "parse name in application" $
        parseLCN "id x" --> Ap (Name "id") (V 'x')
    ]

programTests :: TestTree
programTests =
  testGroup
    "LCProgram Parsing"
    [ testCase "parse program with single definition" $
        parse "id = \\x.(x)\nid"
          --> LCProgram [Def "id" (Ab 'x' (V 'x'))] (Name "id"),
      testCase "parse program with multiple definitions" $
        parse "const = \\x.(\\y.(x))\nid = \\x.(x)\nconst id"
          --> LCProgram
            [ Def "const" (Ab 'x' (Ab 'y' (V 'x'))),
              Def "id" (Ab 'x' (V 'x'))
            ]
            (Ap (Name "const") (Name "id")),
      testCase "parse program with named main" $
        parse "identity = \\x.(x)\nidentity"
          --> LCProgram [Def "identity" (Ab 'x' (V 'x'))] (Name "identity"),
      testCase "parse program with complex main" $
        parse "foo = \\x.(x x)\nfoo foo"
          --> LCProgram
            [Def "foo" (Ab 'x' (Ap (V 'x') (V 'x')))]
            (Ap (Name "foo") (Name "foo")),
      testCase "parse program without definitions" $
        parse "\\x.(x)"
          --> LCProgram [] (Ab 'x' (V 'x'))
    ]

complexTests :: TestTree
complexTests =
  testGroup
    "Complex Expression Parsing"
    [ testCase "parse identity function application" $
        parseLCN "\\x.(x) y" --> Ap (Ab 'x' (V 'x')) (V 'y'),
      testCase "parse multiple abstractions" $
        parseLCN "\\x.(\\y.(x y))" --> Ab 'x' (Ab 'y' (Ap (V 'x') (V 'y'))),
      testCase "parse complex nested expression" $
        parseLCN "\\x.(x \\y.(y) z)"
          --> Ab
            'x'
            (Ap (Ap (V 'x') (Ab 'y' (V 'y'))) (V 'z')),
      testCase
        "parse S combinator like expression"
        $ parseLCN "\\x.(\\y.(\\z.(x z (y z))))"
          --> Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z'))))),
      testCase
        "apply S combinator to left"
        $ parseLCN "\\x.(\\y.(\\z.(x z (y z)))) \\x.(\\y.(x))"
          --> Ap (Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z')))))) (Ab 'x' (Ab 'y' (V 'x'))),
      testCase
        "omega"
        $ parseLCN "\\x.(x x) \\x.(x x)"
          --> Ap (Ab 'x' (Ap (V 'x') (V 'x'))) (Ab 'x' (Ap (V 'x') (V 'x'))),
      testCase
        "apply omega to right"
        $ parseLCN "\\x.(x x) \\x.(x x) \\x.(\\y.(y))"
          --> Ap (Ap (Ab 'x' (Ap (V 'x') (V 'x'))) (Ab 'x' (Ap (V 'x') (V 'x')))) (Ab 'x' (Ab 'y' (V 'y'))),
      testCase "parse expression with mixed variables and names" $
        parseLCN "map f (cons x xs)"
          --> Ap
            (Ap (Name "map") (V 'f'))
            (Ap (Ap (Name "cons") (V 'x')) (Name "xs")),
      testCase "parse complex program with definitions" $
        parse "compose = \\f.(\\g.(\\x.(f (g x))))\nid = \\x.(x)\ncompose id id"
          --> LCProgram
            [ Def "compose" (Ab 'f' (Ab 'g' (Ab 'x' (Ap (V 'f') (Ap (V 'g') (V 'x')))))),
              Def "id" (Ab 'x' (V 'x'))
            ]
            (Ap (Ap (Name "compose") (Name "id")) (Name "id"))
    ]
