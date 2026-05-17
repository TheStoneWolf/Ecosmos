import Test.Tasty
import qualified Tests.HexRam as Tests
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.accumTests
      ]
