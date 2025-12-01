{-# LANGUAGE OverloadedStrings #-}

module MLTyperTest where

import Data.Map qualified as Map
import Expr.ML
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Types.ML hiding ((-->))

-- 测试环境，包含基本类型和常量
testEnv :: Map.Map String MLType
testEnv =
  Map.fromList
    [ ("true", Basic "Bool"),
      ("false", Basic "Bool"),
      ("zero", Basic "Int"),
      ("succ", Arrow (Basic "Int") (Basic "Int")),
      ("pred", Arrow (Basic "Int") (Basic "Int")),
      ("iszero", Arrow (Basic "Int") (Basic "Bool")),
      ("not", Arrow (Basic "Bool") (Basic "Bool")),
      ("and", Arrow (Basic "Bool") (Arrow (Basic "Bool") (Basic "Bool"))),
      ("+", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int"))),
      ("*", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int")))
    ]

-- 辅助函数，运行算法W
runW :: MLTerm -> Maybe MLType
runW = algorithmW testEnv

mlTyperTests :: TestTree
mlTyperTests =
  testGroup
    "ML Type Inference Tests"
    [ constantTypeTests,
      variableTypeTests,
      abstractionTypeTests,
      applicationTypeTests,
      letTypeTests,
      fixTypeTests,
      polymorphismTests,
      complexTypeTests
    ]

constantTypeTests :: TestTree
constantTypeTests =
  testGroup
    "Constant Type Inference"
    [ testCase "type of true" $
        runW (Const "true") --> Just (Basic "Bool"),
      testCase "type of false" $
        runW (Const "false") --> Just (Basic "Bool"),
      testCase "type of zero" $
        runW (Const "zero") --> Just (Basic "Int"),
      testCase "type of succ" $
        runW (Const "succ") --> Just (Arrow (Basic "Int") (Basic "Int")),
      testCase "type of not" $
        runW (Const "not") --> Just (Arrow (Basic "Bool") (Basic "Bool"))
    ]

variableTypeTests :: TestTree
variableTypeTests =
  testGroup
    "Variable Type Inference"
    [ testCase "type of free variable should fail" $
        runW (V "x") --> Nothing,
      testCase "type of bound variable in lambda" $
        runW (Ab "x" (V "x")) --> Just (Arrow (Phi 'a') (Phi 'a')),
      testCase "type of variable in nested lambda" $
        runW (Ab "x" (Ab "y" (V "x")))
          --> Just (Arrow (Phi 'a') (Arrow (Phi 'b') (Phi 'a')))
    ]

abstractionTypeTests :: TestTree
abstractionTypeTests =
  testGroup
    "Abstraction Type Inference"
    [ testCase "type of identity function" $
        runW (Ab "x" (V "x")) --> Just (Arrow (Phi 'a') (Phi 'a')),
      testCase "type of constant function" $
        runW (Ab "x" (Ab "y" (V "x")))
          --> Just (Arrow (Phi 'a') (Arrow (Phi 'b') (Phi 'a'))),
      testCase "type of function with application" $
        runW (Ab "x" (Ap (Const "succ") (V "x")))
          --> Just (Arrow (Basic "Int") (Basic "Int")),
      testCase "type of higher-order function" $
        runW (Ab "f" (Ab "x" (Ap (V "f") (Ap (Const "succ") (V "x")))))
          --> Just
            ( Arrow
                (Arrow (Basic "Int") (Phi 'd'))
                (Arrow (Basic "Int") (Phi 'd'))
            )
    ]

applicationTypeTests :: TestTree
applicationTypeTests =
  testGroup
    "Application Type Inference"
    [ testCase "type of succ zero" $
        runW (Ap (Const "succ") (Const "zero"))
          --> Just (Basic "Int"),
      testCase "type of not true" $
        runW (Ap (Const "not") (Const "true"))
          --> Just (Basic "Bool"),
      testCase "type of and true false" $
        runW (Ap (Ap (Const "and") (Const "true")) (Const "false"))
          --> Just (Basic "Bool"),
      testCase "type of curried application" $
        runW (Ab "f" (Ab "x" (Ab "y" (Ap (Ap (V "f") (V "x")) (V "y")))))
          --> Just
            ( Arrow
                (Arrow (Phi 'b') (Arrow (Phi 'c') (Phi 'e')))
                (Arrow (Phi 'b') (Arrow (Phi 'c') (Phi 'e')))
            )
    ]

letTypeTests :: TestTree
letTypeTests =
  testGroup
    "Let Binding Type Inference"
    [ testCase "type of simple let" $
        runW (Let ("x", Const "zero") (V "x"))
          --> Just (Basic "Int"),
      testCase "type of let with lambda" $
        runW (Let ("id", Ab "x" (V "x")) (V "id"))
          --> Just (Arrow (Phi 'a') (Phi 'a')),
      testCase "type of let with application" $
        runW
          ( Let
              ("f", Const "succ")
              ( Let
                  ("x", Const "zero")
                  (Ap (V "f") (V "x"))
              )
          )
          --> Just (Basic "Int"),
      testCase "polymorphic let (identity)" $
        runW
          ( Let
              ("id", Ab "x" (V "x"))
              (Ap (V "id") (Const "true"))
          )
          --> Just (Basic "Bool"),
      testCase "polymorphic let used twice" $
        runW
          ( Let
              ("id", Ab "x" (V "x"))
              ( Ap
                  ( Ap
                      (Const "and")
                      (Ap (V "id") (Const "true"))
                  )
                  (Ap (V "id") (Const "false"))
              )
          )
          --> Just (Basic "Bool")
    ]

fixTypeTests :: TestTree
fixTypeTests =
  testGroup
    "Fixpoint Type Inference"
    [ testCase "type of divergent fixpoint" $
        runW (Fix "f" (Ab "x" (Ap (V "f") (V "x"))))
          --> Just (Arrow (Phi 'b') (Phi 'c')),
      testCase "type of factorial-like fixpoint" $
        let term =
              Fix
                "fact"
                ( Ab
                    "n"
                    ( Ap
                        ( Ap
                            (Ap (Const "iszero") (V "n"))
                            (Const "zero")
                        )
                        ( Ap
                            (Ap (Const "*") (V "n"))
                            ( Ap
                                (V "fact")
                                (Ap (Const "pred") (V "n"))
                            )
                        )
                    )
                )
         in runW term --> Just (Arrow (Basic "Int") (Basic "Int")),
      testCase "type of mutual recursion (simplified)" $
        runW (Fix "f" (Ab "x" (Ap (V "f") (Ap (Const "succ") (V "x")))))
          --> Just (Arrow (Basic "Int") (Phi 'd'))
    ]

polymorphismTests :: TestTree
polymorphismTests =
  testGroup
    "Polymorphism Tests"
    [ testCase "polymorphic identity application" $
        runW
          ( Let
              ("id", Ab "x" (V "x"))
              (Ap (V "id") (V "id"))
          )
          --> Just (Arrow (Phi 'a') (Phi 'a')),
      testCase "polymorphic constant function" $
        runW
          ( Let
              ("const", Ab "x" (Ab "y" (V "x")))
              (Ap (Ap (V "const") (Const "true")) (Const "zero"))
          )
          --> Just (Basic "Bool"),
      testCase "polymorphic composition" $
        let term =
              Let
                ("compose", Ab "f" (Ab "g" (Ab "x" (Ap (V "f") (Ap (V "g") (V "x"))))))
                (V "compose")
         in runW term
              --> Just
                ( Arrow
                    (Arrow (Phi 'd') (Phi 'e'))
                    ( Arrow
                        (Arrow (Phi 'c') (Phi 'd'))
                        (Arrow (Phi 'c') (Phi 'e'))
                    )
                ),
      testCase "let polymorphism with multiple uses" $
        runW
          ( Let
              ("f", Ab "x" (V "x"))
              ( Ap
                  ( Ap
                      (Const "and")
                      (Ap (V "f") (Const "true"))
                  )
                  (Ap (V "f") (Const "false"))
              )
          )
          --> Just (Basic "Bool")
    ]

complexTypeTests :: TestTree
complexTypeTests =
  testGroup
    "Complex Expression Type Inference"
    [ testCase "type of S combinator" $
        let term =
              Ab
                "x"
                ( Ab
                    "y"
                    ( Ab
                        "z"
                        ( Ap
                            (Ap (V "x") (V "z"))
                            (Ap (V "y") (V "z"))
                        )
                    )
                )
         in runW term
              --> Just
                ( Arrow
                    (Arrow (Phi 'c') (Arrow (Phi 'e') (Phi 'f')))
                    ( Arrow
                        (Arrow (Phi 'c') (Phi 'e'))
                        (Arrow (Phi 'c') (Phi 'f'))
                    )
                ),
      testCase "type of K combinator" $
        runW (Ab "x" (Ab "y" (V "x")))
          --> Just (Arrow (Phi 'a') (Arrow (Phi 'b') (Phi 'a'))),
      testCase "type of Y combinator (simplified)" $
        runW
          ( Ab
              "f"
              ( Ap
                  (Ab "x" (Ap (V "f") (Ap (V "x") (V "x"))))
                  (Ab "x" (Ap (V "f") (Ap (V "x") (V "x"))))
              )
          )
          --> Just (Arrow (Arrow (Phi 'a') (Phi 'a')) (Phi 'a')),
      testCase "type of church numeral 0" $
        runW (Ab "f" (Ab "x" (V "x")))
          --> Just
            ( Arrow
                (Arrow (Phi 'a') (Phi 'b'))
                (Arrow (Phi 'a') (Phi 'a'))
            ),
      testCase "type of church numeral 1" $
        runW (Ab "f" (Ab "x" (Ap (V "f") (V "x"))))
          --> Just
            ( Arrow
                (Arrow (Phi 'a') (Phi 'a'))
                (Arrow (Phi 'a') (Phi 'a'))
            ),
      testCase "type of arithmetic expression" $
        runW
          ( Let
              ("x", Const "zero")
              ( Let
                  ("y", Ap (Const "succ") (V "x"))
                  (Ap (Ap (Const "+") (V "x")) (V "y"))
              )
          )
          --> Just (Basic "Int")
    ]
