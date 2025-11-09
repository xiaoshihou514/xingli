import LCNTyperTest
import LCParserTest
import LCTyperTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ lcParserTests,
        curryTyperTests,
        lcnTests
      ]
