import LCNParserTest
import LCNRTyperTest
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
        lcnParserTests,
        curryTyperTests,
        lcnTests,
        lcnrTests
      ]
