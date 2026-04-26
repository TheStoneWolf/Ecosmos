module Tests.HexRam where

import Clash.Hedgehog.Sized.Unsigned
import qualified Clash.Prelude as C
import qualified Example.Hex as Hex
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HexRam
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

genHexCoord =
  Hex.HexCoord
    <$> genUnsigned (Range.linear 0 5)
    <*> genUnsigned (Range.linear 0 5)
    <*> genUnsigned (Range.linear 0 5)

simDuration = 100

prop_read_ram :: H.Property
prop_read_ram = H.property $ do
  inp <-
    H.forAll (Gen.list (Range.singleton simDuration) genHexCoord)

  -- The output register of the BRAM is undefined on startup, therefore drop it
  let simOut = drop 1 $ fromIntegral <$> C.sampleN (simDuration + 1) (hexRam @C.System @4 (C.fromList inp)) :: [Int]
      expected = replicate simDuration 3

  -- Check that the simulated output matches the expected output
  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
