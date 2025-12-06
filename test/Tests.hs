import LCNParserTest
import LCNRParserTest
import LCNRTyperTest
import LCNTyperTest
import LCParserTest
import LCTyperTest
import MLParserTest
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
        mlParserTests,
        mlTyperTests,
        curryTyperTests,
        lcnTests,
        lcnrTests
      ]
