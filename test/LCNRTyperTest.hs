{-# LANGUAGE OverloadedStrings #-}

module LCNRTyperTest where

import Data.Map qualified as Map
import Expr.LCNR
import TUtils
import Test.Tasty
import Test.Tasty.HUnit
import Types.CurryTypes hiding ((-->))

lcnrTests :: TestTree
lcnrTests =
  testGroup
    "LCNR Type Inference Tests (with Recursion)"
    [ noRecTests,
      recTests
    ]

noRecTests :: TestTree
noRecTests =
  testGroup
    "No rec Type Inference"
    [ testCase "simple recursive identity function" $
        ppln (LCNRProgram [Def "id" (Ab 'x' (V 'x'))] (Name "id"))
          --> Just (TypeCtx (Map.fromList [('x', Phi 'A')]) 'A', Arrow (Phi 'B') (Phi 'B'), Map.singleton "id" (Arrow (Phi 'A') (Phi 'A'), False)),
      testCase "non-recursive definition with RecDef" $
        ppln (LCNRProgram [Def "const" (Ab 'x' (Ab 'y' (V 'x')))] (Name "const"))
          --> Just (TypeCtx (Map.fromList [('x', Phi 'A'), ('y', Phi 'B')]) 'A', Arrow (Phi 'C') (Arrow (Phi 'D') (Phi 'C')), Map.singleton "const" (Arrow (Phi 'A') (Arrow (Phi 'B') (Phi 'A')), False)),
      testCase "recursive function with application" $
        ppln (LCNRProgram [Def "id" (Ab 'x' (V 'x'))] (Ap (Name "id") (V 'y')))
          --> Just (TypeCtx (Map.fromList [('x', Phi 'A'), ('y', Phi 'D')]) 'D', Phi 'D', Map.singleton "id" (Arrow (Phi 'A') (Phi 'A'), False))
    ]

recTests :: TestTree
recTests =
  testGroup
    "Rec Type Inference"
    [ testCase "Y combinator 1" $
        ppln
          ( LCNRProgram
              [ Def "S" (Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z')))))),
                Def "K" (Ab 'x' (Ab 'y' (V 'x'))),
                Def "I" (Ap (Ap (Name "S") (Name "K")) (Name "K")),
                RecDef "Y" (Ab 'm' (Ap (V 'm') (Ap (Name "Y") (V 'm'))))
              ]
              (Ap (Name "Y") (Name "I"))
          )
          --> Just (TypeCtx Map.empty 'A', Phi 'A', Map.empty),
      testCase "Y combinator 2" $
        ppln
          ( LCNRProgram
              [ Def "E" (Ab 'x' (Ab 'y' (Ap (V 'x') (V 'y')))),
                Def "S" (Ab 'x' (Ab 'y' (Ab 'z' (Ap (Ap (V 'x') (V 'z')) (Ap (V 'y') (V 'z')))))),
                RecDef "Y" (Ab 'm' (Ap (V 'm') (Ap (Name "Y") (V 'm'))))
              ]
              (Ap (Name "Y") (Ap (Name "S") (Ap (Name "E") (Name "E"))))
          )
          --> Just (TypeCtx Map.empty 'A', Phi 'A', Map.empty)
    ]
