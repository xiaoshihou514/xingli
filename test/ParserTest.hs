{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import LC
import LCParser
import TUtils
import Test.Tasty
import Test.Tasty.HUnit

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Tests"
    [ variableTests,
      abstractionTests,
      applicationTests,
      complexTests
    ]

variableTests :: TestTree
variableTests =
  testGroup
    "Variable Parsing"
    [ testCase "parse single variable" $
        parse "x" --> V 'x',
      testCase "parse another variable" $
        parse "a" --> V 'a'
    ]

abstractionTests :: TestTree
abstractionTests =
  testGroup
    "Abstraction Parsing"
    [ testCase "parse simple abstraction" $
        parse "\\x.(x)" --> Ab 'x' (V 'x'),
      testCase "parse nested abstraction" $
        parse "\\x.(\\y.(x))" --> Ab 'x' (Ab 'y' (V 'x')),
      testCase "parse abstraction with application inside" $
        parse "\\x.(x x)" --> Ab 'x' (Ap (V 'x') (V 'x'))
    ]

applicationTests :: TestTree
applicationTests =
  testGroup
    "Application Parsing"
    [ testCase "parse simple application" $
        parse "x y" --> Ap (V 'x') (V 'y'),
      testCase "parse left-associative application" $
        parse "x y z" --> Ap (Ap (V 'x') (V 'y')) (V 'z'),
      testCase "parse application with abstraction" $
        parse "\\x.(x) y" --> Ap (Ab 'x' (V 'x')) (V 'y')
    ]

complexTests :: TestTree
complexTests =
  testGroup
    "Complex Expression Parsing"
    [ testCase "parse identity function application" $
        parse "\\x.(x) y" --> Ap (Ab 'x' (V 'x')) (V 'y'),
      testCase "parse multiple abstractions" $
        parse "\\x.(\\y.(x y))" --> Ab 'x' (Ab 'y' (Ap (V 'x') (V 'y'))),
      testCase "parse complex nested expression" $
        parse "\\x.(x \\y.(y) z)"
          --> Ab
            'x'
            (Ap (Ap (V 'x') (Ab 'y' (V 'y'))) (V 'z')),
      testCase
        "parse S combinator like expression"
        $ parse "\\x.(\\y.(\\z.(x z (y z))))"
          --> Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z'))))),
      testCase
        "apply S combinator to left"
        $ parse "\\x.(\\y.(\\z.(x z (y z)))) \\x.(\\y.(x))"
          --> Ap (Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z')))))) (Ab 'x' (Ab 'y' (V 'x'))),
      testCase
        "omega"
        $ parse "\\x.(x x) \\x.(x x)"
          --> Ap (Ab 'x' (Ap (V 'x') (V 'x'))) (Ab 'x' (Ap (V 'x') (V 'x'))),
      testCase
        "apply omega to right"
        $ parse "\\x.(x x) \\x.(x x) \\x.(\\y.(y))"
          --> Ap (Ap (Ab 'x' (Ap (V 'x') (V 'x'))) (Ab 'x' (Ap (V 'x') (V 'x')))) (Ab 'x' (Ab 'y' (V 'y')))
    ]
