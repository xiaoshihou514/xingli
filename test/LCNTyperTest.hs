{-# LANGUAGE OverloadedStrings #-}

module LCNTyperTest where

import Data.Map qualified as Map
import Expr.LCN
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Types.CurryTypes hiding ((-->))

lcnTests :: TestTree
lcnTests =
  testGroup
    "LCN Type Inference Tests"
    [ basicTypeTests,
      definitionTypeTests,
      complexDefinitionTests,
      programTypeTests
    ]

basicTypeTests :: TestTree
basicTypeTests =
  testGroup
    "Basic Type Inference"
    [ testCase "type of identity function with definition" $
        ppln (LCNProgram [Def "id" (Ab 'x' (V 'x'))] (Name "id"))
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'A') (Phi 'A')),
      testCase "type of constant function with definition" $
        ppln (LCNProgram [Def "const" (Ab 'x' (Ab 'y' (V 'x')))] (Name "const"))
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'A'))),
      testCase "type of application using definition" $
        ppln (LCNProgram [Def "id" (Ab 'x' (V 'x'))] (Ap (Name "id") (V 'y')))
          --> Just (TypeCtx (Map.fromList [('y', Phi 'C')]) 'B', Phi 'C')
    ]

definitionTypeTests :: TestTree
definitionTypeTests =
  testGroup
    "Definition Type Inference"
    [ testCase "type of multiple independent definitions" $
        ppln
          ( LCNProgram
              [ Def "id" (Ab 'x' (V 'x')),
                Def "const" (Ab 'x' (Ab 'y' (V 'x')))
              ]
              (Name "const")
          )
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'A'))),
      testCase "type of definition used in another definition" $
        ppln
          ( LCNProgram
              [Def "id" (Ab 'x' (V 'x'))]
              (Ap (Name "id") (V 'z'))
          )
          --> Just (TypeCtx (Map.fromList [('z', Phi 'C')]) 'D', Phi 'C')
    ]

complexDefinitionTests :: TestTree
complexDefinitionTests =
  testGroup
    "Complex Definition Type Inference"
    [ testCase "type of composition with definitions" $
        ppln
          ( LCNProgram
              [ Def "compose" (Ab 'f' (Ab 'g' (Ab 'x' (Ap (V 'f') (Ap (V 'g') (V 'x')))))),
                Def "id" (Ab 'x' (V 'x'))
              ]
              (Ap (Ap (Name "compose") (Name "id")) (Name "id"))
          )
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'F') (Phi 'F')),
      testCase "type of S combinator with definitions" $
        ppln
          ( LCNProgram
              [ Def "s" (Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z'))))))
              ]
              (Name "s")
          )
          --> Just
            ( TypeCtx Map.empty 'A',
              Arrow
                (Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'C')))
                ( Arrow
                    (Arrow (Phi 'A') (Phi 'B'))
                    (Arrow (Phi 'A') (Phi 'C'))
                )
            )
    ]

programTypeTests :: TestTree
programTypeTests =
  testGroup
    "Complete LCNProgram Type Inference"
    [ testCase "type of complete functional program" $
        ( fmap snd $
            ppln
              ( LCNProgram
                  [ Def "true" (Ab 'x' (Ab 'y' (V 'x'))), -- A -> B -> A
                    Def "false" (Ab 'x' (Ab 'y' (V 'y'))), -- A -> B -> B
                    Def "if" (Ab 'p' (Ab 'a' (Ab 'b' (Ap (Ap (V 'p') (V 'a')) (V 'b'))))) -- (A -> B -> C) -> A -> B -> C
                  ]
                  $ Ap
                    (Ab 'b' (Ap (Ap (Ap (Name "if") (V 'b')) (Name "false")) (Name "true")))
                    (Name "true")
              )
        )
          --> Just (Arrow (Phi 'F') (Arrow (Phi 'G') (Phi 'G'))),
      testCase "type of church numeral program" $
        ppln
          ( LCNProgram
              [ Def "zero" (Ab 'f' (Ab 'x' (V 'x'))),
                Def "succ" (Ab 'n' (Ab 'f' (Ab 'x' (Ap (V 'f') (Ap (Ap (V 'n') (V 'f')) (V 'x')))))),
                Def "plus" (Ab 'm' (Ab 'n' (Ab 'f' (Ab 'x' (Ap (Ap (V 'm') (V 'f')) (Ap (Ap (V 'n') (V 'f')) (V 'x')))))))
              ]
              (Ap (Ap (Name "plus") (Name "zero")) (Ap (Name "succ") (Name "zero")))
          )
          --> Just
            ( TypeCtx Map.empty 'A',
              Arrow
                (Arrow (Phi 'L') (Phi 'I'))
                (Arrow (Phi 'L') (Phi 'I'))
            ),
      testCase "type of complex program with mixed definitions" $
        ppln
          ( LCNProgram
              [ Def "id" (Ab 'x' (V 'x')),
                Def "apply" (Ab 'f' (Ab 'x' (Ap (V 'f') (V 'x')))),
                Def "compose" (Ab 'f' (Ab 'g' (Ab 'x' (Ap (V 'f') (Ap (V 'g') (V 'x'))))))
              ]
              (Ap (Ap (Name "compose") (Name "id")) (Name "id"))
          )
          --> Just (TypeCtx Map.empty 'A', Arrow (Phi 'F') (Phi 'F'))
    ]
