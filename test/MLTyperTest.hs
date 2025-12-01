{-# LANGUAGE OverloadedStrings #-}

module MLTyperTest where

import Data.Map qualified as Map
import Expr.ML
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Types.ML hiding ((-->))

-- 测试环境，只包含基础构件
testEnv :: Map.Map String MLType
testEnv =
  Map.fromList
    [ -- 基本类型和函数
      ("true", Basic "Bool"),
      ("false", Basic "Bool"),
      ("zero", Basic "Int"),
      ("one", Basic "Int"),
      ("succ", Arrow (Basic "Int") (Basic "Int")),
      ("pred", Arrow (Basic "Int") (Basic "Int")),
      ("iszero", Arrow (Basic "Int") (Basic "Bool")),
      ("not", Arrow (Basic "Bool") (Basic "Bool")),
      ("and", Arrow (Basic "Bool") (Arrow (Basic "Bool") (Basic "Bool"))),
      ("or", Arrow (Basic "Bool") (Arrow (Basic "Bool") (Basic "Bool"))),
      ("+", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int"))),
      ("*", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int"))),
      ("-", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int"))),
      -- 多态条件函数
      ("if", Qtf 'a' (Arrow (Basic "Bool") (Arrow (Phi 'a') (Arrow (Phi 'a') (Phi 'a'))))),
      -- 列表基础构件（多态）
      ("Nil", Qtf 'a' (Basic "List")),
      ("Cons", Qtf 'a' (Arrow (Phi 'a') (Arrow (Basic "List") (Basic "List"))))
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
      listConstructionTests,
      listFunctionDefinitionTests,
      recursiveArithmeticTests,
      complexTypeTests
    ]

-- 基础测试保持不变
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
      testCase "type of Nil (polymorphic)" $
        runW (Const "Nil") --> Just (Basic "List"),
      testCase "type of Cons (polymorphic)" $
        runW (Const "Cons")
          --> Just (Arrow (Phi 'a') (Arrow (Basic "List") (Basic "List")))
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
          --> Just (Arrow (Basic "Int") (Basic "Int"))
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
          --> Just (Basic "Bool")
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
          --> Just (Arrow (Phi 'b') (Phi 'b')),
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
      testCase "type of factorial using fix" $
        let term =
              Fix
                "fact"
                (Ab "n" (Ap (Ap (Ap (Const "if") (Ap (Const "iszero") (V "n"))) (Const "one")) (Ap (Ap (Const "*") (V "n")) (Ap (V "fact") (Ap (Const "pred") (V "n"))))))
         in runW term --> Just (Arrow (Basic "Int") (Basic "Int"))
    ]

polymorphismTests :: TestTree
polymorphismTests =
  testGroup
    "Polymorphism Tests"
    [ testCase "polymorphic if with ints" $
        runW (Ap (Ap (Ap (Const "if") (Const "true")) (Const "zero")) (Const "one"))
          --> Just (Basic "Int"),
      testCase "polymorphic if with bools" $
        runW (Ap (Ap (Ap (Const "if") (Const "true")) (Const "true")) (Const "false"))
          --> Just (Basic "Bool"),
      testCase "polymorphic if with identity functions" $
        let term =
              Ap
                ( Ap
                    (Ap (Const "if") (Const "true"))
                    (Ab "x" (V "x"))
                )
                (Ab "y" (V "y"))
         in runW term --> Just (Arrow (Phi 'e') (Phi 'e')),
      testCase "polymorphic if type mismatch should fail" $
        let term =
              Ap
                ( Ap
                    (Ap (Const "if") (Const "true"))
                    (Const "zero")
                )
                (Const "false")
         in runW term --> Nothing
    ]

listConstructionTests :: TestTree
listConstructionTests =
  testGroup
    "List Construction Tests"
    [ testCase "type of empty list" $
        runW (Const "Nil") --> Just (Basic "List"),
      testCase "type of singleton int list" $
        runW (Ap (Ap (Const "Cons") (Const "zero")) (Const "Nil"))
          --> Just (Basic "List"),
      testCase "type of three-element int list" $
        let term =
              Ap
                ( Ap
                    (Const "Cons")
                    (Const "zero")
                )
                ( Ap
                    ( Ap
                        (Const "Cons")
                        (Const "one")
                    )
                    ( Ap
                        ( Ap
                            (Const "Cons")
                            (Ap (Const "succ") (Const "one"))
                        )
                        (Const "Nil")
                    )
                )
         in runW term --> Just (Basic "List"),
      testCase "type of bool list" $
        let term =
              Ap
                ( Ap
                    (Const "Cons")
                    (Const "true")
                )
                ( Ap
                    ( Ap
                        (Const "Cons")
                        (Const "false")
                    )
                    (Const "Nil")
                )
         in runW term --> Just (Basic "List"),
      testCase "type of polymorphic list constructor" $
        let term = Ab "x" (Ap (Ap (Const "Cons") (V "x")) (Const "Nil"))
         in runW term --> Just (Arrow (Phi 'a') (Basic "List"))
    ]

listFunctionDefinitionTests :: TestTree
listFunctionDefinitionTests =
  testGroup
    "List Function Definition Tests"
    [ testCase "type of length function definition" $
        let term =
              Let ("length", Fix "length" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Ap (Const "isNil") (V "xs")) (Const "true"))) (Const "zero")) (Ap (Const "succ") (Ap (V "length") (Ap (Const "tail") (V "xs"))))))) (V "length")
         in runW term --> Nothing, -- 因为环境中没有isNil和tail
      testCase "type of map function definition (without helper functions)" $
        let term =
              Let ("map", Ab "f" (Fix "map" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Ab "dummy" (Const "true")) (V "xs"))) (Const "Nil")) (Ap (Ap (Const "Cons") (Ap (V "f") (Ap (Const "head") (V "xs")))) (Ap (V "map") (Ap (Const "tail") (V "xs")))))))) (V "map")
         in runW term --> Nothing -- 因为环境中没有head和tail
    ]

-- 添加基础列表操作函数到环境的新版本
testEnvWithListOps :: Map.Map String MLType
testEnvWithListOps =
  Map.union
    testEnv
    ( Map.fromList
        [ ("isNil", Qtf 'a' (Arrow (Basic "List") (Basic "Bool"))),
          ("head", Qtf 'a' (Arrow (Basic "List") (Phi 'a'))),
          ("tail", Qtf 'a' (Arrow (Basic "List") (Basic "List")))
        ]
    )

runWWithListOps :: MLTerm -> Maybe MLType
runWWithListOps = algorithmW testEnvWithListOps

listFunctionDefinitionTestsWithOps :: TestTree
listFunctionDefinitionTestsWithOps =
  testGroup
    "List Function Definition Tests (with list ops)"
    [ testCase "type of length function" $
        let term = Let ("length", Fix "length" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Const "isNil") (V "xs"))) (Const "zero")) (Ap (Const "succ") (Ap (V "length") (Ap (Const "tail") (V "xs"))))))) (V "length") in runWWithListOps term --> Just (Arrow (Basic "List") (Basic "Int")),
      testCase "type of map function" $
        let term = Let ("map", Ab "f" (Fix "map" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Const "isNil") (V "xs"))) (Const "Nil")) (Ap (Ap (Const "Cons") (Ap (V "f") (Ap (Const "head") (V "xs")))) (Ap (V "map") (Ap (Const "tail") (V "xs")))))))) (V "map")
         in runWWithListOps term
              --> Just
                ( Arrow
                    (Arrow (Phi 'a') (Phi 'b'))
                    (Arrow (Basic "List") (Basic "List"))
                ),
      testCase "type of foldr function" $
        let term = Let ("foldr", Ab "f" (Ab "z" (Fix "fold" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Const "isNil") (V "xs"))) (V "z")) (Ap (Ap (V "f") (Ap (Const "head") (V "xs"))) (Ap (Ap (V "fold") (Ap (Const "tail") (V "xs"))) (V "z")))))))) (V "foldr")
         in runWWithListOps term
              --> Just
                ( Arrow
                    (Arrow (Phi 'a') (Arrow (Phi 'b') (Phi 'b')))
                    ( Arrow
                        (Phi 'b')
                        (Arrow (Basic "List") (Phi 'b'))
                    )
                ),
      testCase "type of sum using foldr" $
        let term =
              Let
                ("foldr", Ab "f" (Ab "z" (Fix "fold" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Const "isNil") (V "xs"))) (V "z")) (Ap (Ap (V "f") (Ap (Const "head") (V "xs"))) (Ap (Ap (V "fold") (Ap (Const "tail") (V "xs"))) (V "z"))))))))
                ( Let
                    ("sum", Ap (Ap (V "foldr") (Const "+")) (Const "zero"))
                    (V "sum")
                )
         in runWWithListOps term --> Just (Arrow (Basic "List") (Basic "Int")),
      testCase "type of map and foldr composition" $
        let term = Let ("map", Ab "f" (Fix "map" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Const "isNil") (V "xs"))) (Const "Nil")) (Ap (Ap (Const "Cons") (Ap (V "f") (Ap (Const "head") (V "xs")))) (Ap (V "map") (Ap (Const "tail") (V "xs")))))))) (Let ("foldr", Ab "f" (Ab "z" (Fix "fold" (Ab "xs" (Ap (Ap (Ap (Const "if") (Ap (Const "isNil") (V "xs"))) (V "z")) (Ap (Ap (V "f") (Ap (Const "head") (V "xs"))) (Ap (Ap (V "fold") (Ap (Const "tail") (V "xs"))) (V "z")))))))) (Let ("sumSquares", Ab "xs" (Ap (Ap (Ap (V "foldr") (Const "+")) (Const "zero")) (Ap (Ap (V "map") (Ab "x" (Ap (Ap (Const "*") (V "x")) (V "x")))) (V "xs")))) (V "sumSquares")))
         in runWWithListOps term --> Just (Arrow (Basic "List") (Basic "Int"))
    ]

recursiveArithmeticTests :: TestTree
recursiveArithmeticTests =
  testGroup
    "Recursive Arithmetic Tests"
    [ testCase "type of recursive addition" $
        let term = Let ("add", Fix "add" (Ab "x" (Ab "y" (Ap (Ap (Ap (Const "if") (Ap (Const "iszero") (V "x"))) (V "y")) (Ap (Ap (V "add") (Ap (Const "pred") (V "x"))) (Ap (Const "succ") (V "y"))))))) (Ap (Ap (V "add") (Const "zero")) (Const "zero"))
         in runW term --> Just (Basic "Int"),
      testCase "type of recursive multiplication" $
        let term = Let ("mult", Fix "mult" (Ab "x" (Ab "y" (Ap (Ap (Ap (Const "if") (Ap (Const "iszero") (V "x"))) (Const "zero")) (Ap (Ap (Const "+") (V "y")) (Ap (Ap (V "mult") (Ap (Const "pred") (V "x"))) (V "y"))))))) (Ap (Ap (V "mult") (Const "zero")) (Const "zero"))
         in runW term --> Just (Basic "Int"),
      testCase "type of factorial using let" $
        let term = Let ("fact", Fix "fact" (Ab "n" (Ap (Ap (Ap (Const "if") (Ap (Const "iszero") (V "n"))) (Const "one")) (Ap (Ap (Const "*") (V "n")) (Ap (V "fact") (Ap (Const "pred") (V "n"))))))) (Ap (V "fact") (Const "zero"))
         in runW term --> Just (Basic "Int"),
      testCase "type of combined arithmetic functions" $
        let term = Let ("add", Fix "add" (Ab "x" (Ab "y" (Ap (Ap (Ap (Const "if") (Ap (Const "iszero") (V "x"))) (V "y")) (Ap (Ap (V "add") (Ap (Const "pred") (V "x"))) (Ap (Const "succ") (V "y"))))))) (Let ("mult", Fix "mult" (Ab "x" (Ab "y" (Ap (Ap (Ap (Const "if") (Ap (Const "iszero") (V "x"))) (Const "zero")) (Ap (Ap (Const "+") (V "y")) (Ap (Ap (V "mult") (Ap (Const "pred") (V "x"))) (V "y"))))))) (Let ("fact", Fix "fact" (Ab "n" (Ap (Ap (Ap (Const "if") (Ap (Const "iszero") (V "n"))) (Const "one")) (Ap (Ap (Const "*") (V "n")) (Ap (V "fact") (Ap (Const "pred") (V "n"))))))) (Ap (Ap (V "mult") (Ap (V "fact") (Const "zero"))) (Ap (Ap (V "add") (Const "zero")) (Const "one")))))
         in runW term --> Just (Basic "Int")
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
      testCase "type of complex higher-order composition" $
        let term =
              Ab
                "f"
                ( Ab
                    "g"
                    ( Ab
                        "h"
                        ( Ab
                            "x"
                            (Ap (V "f") (Ap (V "g") (Ap (V "h") (V "x"))))
                        )
                    )
                )
         in runW term
              --> Just
                ( Arrow
                    (Arrow (Phi 'f') (Phi 'g')) -- f g
                    ( Arrow
                        (Arrow (Phi 'e') (Phi 'f')) -- e f
                        ( Arrow
                            (Arrow (Phi 'd') (Phi 'e')) -- d e
                            (Arrow (Phi 'd') (Phi 'g')) -- d g
                        )
                    )
                ),
      testCase "type of church-like numeral 2" $
        let term = Ab "f" (Ab "x" (Ap (V "f") (Ap (V "f") (V "x"))))
         in runW term --> Just (Arrow (Arrow (Phi 'd') (Phi 'd')) (Arrow (Phi 'd') (Phi 'd')))
    ]
