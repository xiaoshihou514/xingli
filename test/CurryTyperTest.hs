{-# LANGUAGE OverloadedStrings #-}

module CurryTyperTest where

import CurryTypes hiding ((-->))
import Data.Map qualified as Map
import LDef
import TUtils
import Test.Tasty
import Test.Tasty.HUnit

curryTyperTests :: TestTree
curryTyperTests =
  testGroup
    "Curry Type Inference Tests"
    [ variableTypeTests,
      abstractionTypeTests,
      applicationTypeTests,
      complexTypeTests
    ]

variableTypeTests :: TestTree
variableTypeTests =
  testGroup
    "Variable Type Inference"
    [ testCase "type of single variable" $
        pp (V 'x') --> (TypeCtx (Map.fromList [('x', Phi 'A')]) 'B', Phi 'A'),
      testCase "type of another variable" $
        pp (V 'y') --> (TypeCtx (Map.fromList [('y', Phi 'A')]) 'B', Phi 'A')
    ]

abstractionTypeTests :: TestTree
abstractionTypeTests =
  testGroup
    "Abstraction Type Inference"
    [ testCase "type of identity function" $
        pp (Ab 'x' (V 'x')) --> (TypeCtx (Map.fromList [('x', Phi 'A')]) 'B', Arrow (Phi 'A') (Phi 'A')),
      testCase "type of constant function" $
        pp (Ab 'x' (Ab 'y' (V 'x')))
          --> ( TypeCtx (Map.fromList [('x', Phi 'A'), ('y', Phi 'B')]) 'C',
                Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'A'))
              )
    ]

applicationTypeTests :: TestTree
applicationTypeTests =
  testGroup
    "Application Type Inference"
    [ testCase "type of simple application" $
        pp (Ap (V 'f') (V 'x'))
          --> (TypeCtx (Map.fromList [('f', Arrow (Phi 'B') (Phi 'C')), ('x', Phi 'B')]) 'C', Phi 'C'),
      testCase "type of left-associative application" $
        pp (Ap (Ap (V 'f') (V 'x')) (V 'y'))
          --> ( TypeCtx
                  ( Map.fromList
                      [ ('f', Arrow (Phi 'B') (Arrow (Phi 'D') (Phi 'E'))),
                        ('x', Phi 'B'),
                        ('y', Phi 'D')
                      ]
                  )
                  'F',
                Phi 'E'
              ),
      testCase "type of application with abstraction" $
        pp (Ap (Ab 'x' (V 'x')) (V 'y'))
          --> (TypeCtx (Map.fromList [('x', Phi 'C'), ('y', Phi 'C')]) 'C', Phi 'C')
    ]

complexTypeTests :: TestTree
complexTypeTests =
  testGroup
    "Complex Expression Type Inference"
    [ testCase "type of S combinator" $
        let term = Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z')))))
            expectedCtx =
              TypeCtx
                ( Map.fromList
                    [ ('x', Arrow (Phi 'E') (Arrow (Phi 'F') (Phi 'G'))),
                      ('y', Arrow (Phi 'E') (Phi 'F')),
                      ('z', Phi 'E')
                    ]
                )
                'E'
            expectedType =
              Arrow
                (Arrow (Phi 'E') (Arrow (Phi 'F') (Phi 'G')))
                ( Arrow
                    (Arrow (Phi 'E') (Phi 'F'))
                    (Arrow (Phi 'E') (Phi 'G'))
                )
         in pp term --> (expectedCtx, expectedType),
      testCase "type of K combinator" $
        let term = Ab 'x' (Ab 'y' (V 'x'))
            expectedCtx = TypeCtx (Map.fromList [('x', Phi 'A'), ('y', Phi 'B')]) 'C'
            expectedType = Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'A'))
         in pp term --> (expectedCtx, expectedType),
      testCase "type of function composition" $
        let term = Ab 'f' (Ab 'g' (Ab 'x' (Ap (V 'f') (Ap (V 'g') (V 'x')))))
            expectedCtx =
              TypeCtx
                ( Map.fromList
                    [ ('f', Arrow (Phi 'D') (Phi 'E')),
                      ('g', Arrow (Phi 'C') (Phi 'D')),
                      ('x', Phi 'C')
                    ]
                )
                'E'
            expectedType =
              Arrow
                (Arrow (Phi 'D') (Phi 'E'))
                ( Arrow
                    (Arrow (Phi 'C') (Phi 'D'))
                    (Arrow (Phi 'C') (Phi 'E'))
                )
         in pp term --> (expectedCtx, expectedType)
    ]

-- Helper function for pretty printing in tests
prettyTypeTest :: Term -> String
prettyTypeTest term = prettyPP (pp term)

-- Additional test cases that demonstrate the type inference
demonstrationTests :: TestTree
demonstrationTests =
  testGroup
    "Type Inference Demonstrations"
    [ testCase "demonstrate identity function type" $
        prettyTypeTest (Ab 'x' (V 'x'))
          --> "env:\nx: A\nA -> A",
      testCase "demonstrate function application type" $
        prettyTypeTest (Ap (Ab 'x' (V 'x')) (V 'y'))
          --> "env:\nx: A\ny: A\nA",
      testCase "demonstrate multiple abstractions" $
        prettyTypeTest (Ab 'x' (Ab 'y' (V 'x')))
          --> "env:\nx: A\ny: B\nA -> B -> A"
    ]
