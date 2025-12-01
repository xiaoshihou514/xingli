import LCNParserTest
import LCNRParserTest
import LCNRTyperTest
import LCNTyperTest
import LCParserTest
import LCTyperTest
import MLTyperTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ lcParserTests,
        lcnParserTests,
        lcnrParserTests,
        mlTyperTests,
        curryTyperTests,
        lcnTests,
        lcnrTests
      ]
