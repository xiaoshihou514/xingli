{-# LANGUAGE OverloadedStrings #-}

module MLParserTest where

import Expr.ML
import Parser.ML ()
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Typeclasses

mlParserTests :: TestTree
mlParserTests =
  testGroup
    "ML Parser Tests"
    [ simpleExpressionTests,
      abstractionTests,
      applicationTests,
      fixExpressionTests,
      complexExpressionTests,
      letBindingTests
    ]

simpleExpressionTests :: TestTree
simpleExpressionTests =
  testGroup
    "Simple Expression Parsing"
    [ testCase "parse multi-character constant" $
        parse "succ" --> Const "succ"
    ]

abstractionTests :: TestTree
abstractionTests =
  testGroup
    "Abstraction Parsing"
    [ testCase "parse simple abstraction" $
        parse "\\x.x" --> Ab "x" (V "x"),
      testCase "parse nested abstraction" $
        parse "\\f.\\x.f x" --> Ab "f" (Ab "x" (Ap (V "f") (V "x"))),
      testCase "parse abstraction with constant in body" $
        parse "\\x.succ x" --> Ab "x" (Ap (Const "succ") (V "x")),
      testCase "parse abstraction with parentheses" $
        parse "\\x.(x x)" --> Ab "x" (Ap (V "x") (V "x"))
    ]

applicationTests :: TestTree
applicationTests =
  testGroup
    "Application Parsing"
    [ testCase "parse simple application with multi-char constant" $
        parse "succ x" --> Ap (Const "succ") (V "x"),
      testCase "parse left-associative application" $
        parse "f x y" --> Ap (Ap (V "f") (V "x")) (V "y"),
      testCase "parse application with abstraction" $
        parse "(\\x.x) y" --> Ap (Ab "x" (V "x")) (V "y"),
      testCase "parse complex application chain" $
        parse "f (g x) (h y)"
          --> Ap (Ap (V "f") (Ap (V "g") (V "x"))) (Ap (V "h") (V "y"))
    ]

fixExpressionTests :: TestTree
fixExpressionTests =
  testGroup
    "Fix Expression Parsing"
    [ testCase "parse simple fix expression" $
        parse "fix f.\\x.f x" --> Fix "f" (Ab "x" (Ap (V "f") (V "x"))),
      testCase "parse fix with constant in body" $
        parse "fix fact.\\n.if (zero n) one (mult n (fact (pred n)))"
          --> Fix
            "fact"
            ( Ab
                "n"
                ( Ap
                    ( Ap
                        ( Ap
                            (Const "if")
                            (Ap (Const "zero") (V "n"))
                        )
                        (Const "one")
                    )
                    ( Ap
                        ( Ap
                            (Const "mult")
                            (V "n")
                        )
                        ( Ap
                            (V "fact")
                            (Ap (Const "pred") (V "n"))
                        )
                    )
                )
            ),
      testCase "parse fix with parentheses" $
        parse "fix omega.(\\x.x x) (\\x.x x)"
          --> Fix "omega" (Ap (Ab "x" (Ap (V "x") (V "x"))) (Ab "x" (Ap (V "x") (V "x"))))
    ]

complexExpressionTests :: TestTree
complexExpressionTests =
  testGroup
    "Complex Expression Parsing"
    [ testCase "parse Y combinator" $
        parse "\\f.(\\x.f (x x)) (\\x.f (x x))"
          --> Ab
            "f"
            ( Ap
                (Ab "x" (Ap (V "f") (Ap (V "x") (V "x"))))
                (Ab "x" (Ap (V "f") (Ap (V "x") (V "x"))))
            ),
      testCase "parse church numeral two" $
        parse "\\f.\\x.f (f x)"
          --> Ab "f" (Ab "x" (Ap (V "f") (Ap (V "f") (V "x")))),
      testCase "parse nested applications with abstractions" $
        parse "(\\f.\\x.f x) (\\y.y) z"
          --> Ap
            ( Ap
                (Ab "f" (Ab "x" (Ap (V "f") (V "x"))))
                (Ab "y" (V "y"))
            )
            (V "z")
    ]

letBindingTests :: TestTree
letBindingTests =
  testGroup
    "Top-level Let Binding Parsing"
    [ testCase "parse single let binding" $
        parse "let x = y in\nx"
          --> Let ("x", V "y") (V "x"),
      testCase "parse multiple let bindings" $
        parse "let x = y in\nlet f = \\x.x in\nf x"
          --> Let
            ("x", V "y")
            ( Let
                ("f", Ab "x" (V "x"))
                (Ap (V "f") (V "x"))
            ),
      testCase "parse let with fix expression" $
        parse "let fact = fix f.\\n.if (zero n) one (mult n (f (pred n))) in\nfact five"
          --> Let ("fact", Fix "f" (Ab "n" (Ap (Ap (Ap (Const "if") (Ap (Const "zero") (V "n"))) (Const "one")) (Ap (Ap (Const "mult") (V "n")) (Ap (V "f") (Ap (Const "pred") (V "n"))))))) (Ap (V "fact") (Const "five")),
      testCase "parse let with complex expression" $
        parse "let id = \\x.x in\nlet const = \\x.\\y.x in\nconst id id"
          --> Let
            ("id", Ab "x" (V "x"))
            ( Let
                ("const", Ab "x" (Ab "y" (V "x")))
                (Ap (Ap (V "const") (V "id")) (V "id"))
            ),
      testCase "parse let with free variables" $
        parse "let add = \\x.\\y.plus x y in\nadd one two"
          --> Let
            ( "add",
              Ab
                "x"
                ( Ab
                    "y"
                    (Ap (Ap (Const "plus") (V "x")) (V "y"))
                )
            )
            (Ap (Ap (V "add") (Const "one")) (Const "two"))
    ]
