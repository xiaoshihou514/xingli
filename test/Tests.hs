import CurryTyperTest
import ParserTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ parserTests,
        curryTyperTests
      ]
