{-# LANGUAGE OverloadedStrings #-}

module LCNTyperTest where

import Data.Map qualified as Map
import Expr.LC (TypeCtx (..))
import Expr.LCN
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Types.CurryTypes hiding ((-->))

lcnTests :: TestTree
lcnTests =
  testGroup
    "LCN Type Inference Tests"
    [ variableTypeTests,
      abstractionTypeTests,
      applicationTypeTests,
      nameTypeTests,
      complexTypeTests,
      programTypeTests
    ]

variableTypeTests :: TestTree
variableTypeTests =
  testGroup
    "Variable Type Inference"
    [ testCase "type of single variable" $
        ppln (LCProgram [] (V 'x'))
          --> Just (TypeCtx (Map.fromList [('x', Phi 'A')]) 'B', Phi 'A'),
      testCase "type of another variable" $
        ppln (LCProgram [] (V 'y'))
          --> Just (TypeCtx (Map.fromList [('y', Phi 'A')]) 'B', Phi 'A')
    ]

abstractionTypeTests :: TestTree
abstractionTypeTests =
  testGroup
    "Abstraction Type Inference"
    [ testCase "type of identity function" $
        ppln (LCProgram [] (Ab 'x' (V 'x')))
          --> Just (TypeCtx (Map.fromList [('x', Phi 'A')]) 'B', Arrow (Phi 'A') (Phi 'A')),
      testCase "type of constant function" $
        ppln (LCProgram [] (Ab 'x' (Ab 'y' (V 'x'))))
          --> Just
            ( TypeCtx (Map.fromList [('x', Phi 'A'), ('y', Phi 'B')]) 'C',
              Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'A'))
            )
    ]

applicationTypeTests :: TestTree
applicationTypeTests =
  testGroup
    "Application Type Inference"
    [ testCase "type of simple application" $
        ppln (LCProgram [] (Ap (V 'f') (V 'x')))
          --> Just (TypeCtx (Map.fromList [('f', Arrow (Phi 'B') (Phi 'C')), ('x', Phi 'B')]) 'C', Phi 'C'),
      testCase "type of left-associative application" $
        ppln (LCProgram [] (Ap (Ap (V 'f') (V 'x')) (V 'y')))
          --> Just
            ( TypeCtx
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
        ppln (LCProgram [] (Ap (Ab 'x' (V 'x')) (V 'y')))
          --> Just (TypeCtx (Map.fromList [('x', Phi 'C'), ('y', Phi 'C')]) 'C', Phi 'C')
    ]

nameTypeTests :: TestTree
nameTypeTests =
  testGroup
    "Name Type Inference"
    [ testCase "type of simple named definition" $
        ppln (LCProgram [Def "id" (Ab 'x' (V 'x'))] (Name "id"))
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'A') (Phi 'A')),
      testCase "type of named definition in application" $
        ppln (LCProgram [Def "id" (Ab 'x' (V 'x'))] (Ap (Name "id") (V 'y')))
          --> Just (TypeCtx (Map.fromList [('y', Phi 'C')]) 'B', Phi 'C'),
      testCase "type of multiple named definitions" $
        ppln
          ( LCProgram
              [ Def "const" (Ab 'x' (Ab 'y' (V 'x'))),
                Def "id" (Ab 'x' (V 'x'))
              ]
              (Ap (Name "const") (Name "id"))
          )
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'B') (Arrow (Phi 'C') (Phi 'C')))
    ]

complexTypeTests :: TestTree
complexTypeTests =
  testGroup
    "Complex Expression Type Inference"
    [ testCase "type of S combinator" $
        let term = Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z')))))
         in ppln (LCProgram [] term)
              --> Just
                ( TypeCtx
                    ( Map.fromList
                        [ ('x', Arrow (Phi 'E') (Arrow (Phi 'F') (Phi 'G'))),
                          ('y', Arrow (Phi 'E') (Phi 'F')),
                          ('z', Phi 'E')
                        ]
                    )
                    'H',
                  Arrow
                    (Arrow (Phi 'E') (Arrow (Phi 'F') (Phi 'G')))
                    ( Arrow
                        (Arrow (Phi 'E') (Phi 'F'))
                        (Arrow (Phi 'E') (Phi 'G'))
                    )
                ),
      testCase "type of K combinator" $
        let term = Ab 'x' (Ab 'y' (V 'x'))
         in ppln (LCProgram [] term)
              --> Just
                ( TypeCtx (Map.fromList [('x', Phi 'A'), ('y', Phi 'B')]) 'C',
                  Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'A'))
                ),
      testCase "type of function composition" $
        let term = Ab 'f' (Ab 'g' (Ab 'x' (Ap (V 'f') (Ap (V 'g') (V 'x')))))
         in ppln (LCProgram [] term)
              --> Just
                ( TypeCtx
                    ( Map.fromList
                        [ ('f', Arrow (Phi 'D') (Phi 'E')),
                          ('g', Arrow (Phi 'C') (Phi 'D')),
                          ('x', Phi 'C')
                        ]
                    )
                    'F',
                  Arrow
                    (Arrow (Phi 'D') (Phi 'E'))
                    ( Arrow
                        (Arrow (Phi 'C') (Phi 'D'))
                        (Arrow (Phi 'C') (Phi 'E'))
                    )
                )
    ]

programTypeTests :: TestTree
programTypeTests =
  testGroup
    "LCProgram Type Inference"
    [ testCase "type of program with recursive definition" $
        ppln
          ( LCProgram
              [Def "id" (Ab 'x' (V 'x'))]
              (Ap (Name "id") (Name "id"))
          )
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'B') (Phi 'B')),
      testCase "type of program with multiple uses of same definition" $
        ppln
          ( LCProgram
              [Def "const" (Ab 'x' (Ab 'y' (V 'x')))]
              (Ap (Ap (Name "const") (V 'a')) (V 'b'))
          )
          --> Just (TypeCtx (Map.fromList [('a', Phi 'F'), ('b', Phi 'E')]) 'C', Phi 'F'),
      testCase "type of nested program with definitions" $
        ppln
          ( LCProgram
              [ Def "id" (Ab 'x' (V 'x')),
                Def "apply" (Ab 'f' (Ab 'x' (Ap (V 'f') (V 'x'))))
              ]
              (Ap (Ap (Name "apply") (Name "id")) (V 'z'))
          )
          --> Just (TypeCtx (Map.fromList [('z', Phi 'F')]) 'B', Phi 'F')
    ]
