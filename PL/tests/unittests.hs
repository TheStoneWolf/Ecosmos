import Test.Tasty
import qualified Tests.HexRam as HexRamTests
import qualified Tests.Vegetation as VegetationTests
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ HexRamTests.accumTests,
        VegetationTests.vegetationTests
      ]
