module TUtils where

import Test.Tasty.HUnit (Assertion, HasCallStack, (@?=))

-- IC.Exact.-->

-- |
-- This function ensures that its first argument is the same as the second one.
infix 1 -->

(-->) ::
  (Show a, Eq a, HasCallStack) =>
  -- | the actual value
  a ->
  -- | the expected value
  a ->
  Assertion
(-->) = (@?=)
