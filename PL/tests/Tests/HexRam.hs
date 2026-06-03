module Tests.HexRam where

import Clash.Hedgehog.Sized.Unsigned
import qualified Clash.Prelude as C
import qualified Clash.Shockwaves as SW
import Data.Text.IO (writeFile)
import qualified Example.Hex as Hex
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HexRam
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude hiding (writeFile)

C.createDomain C.vSystem {C.vName = "TestDom", C.vPeriod = 1000}

type AddrWidth = 4

-- TODO Fix why the highest index element is not allowed in the bram
vInAddrRange :: H.Gen (C.Unsigned AddrWidth)
vInAddrRange = genUnsigned (Range.linear 0 ((maxBound :: C.Unsigned AddrWidth) - 1))

type DataD = 4

genHexCoord :: H.Gen (Hex.HexCoord (C.Unsigned AddrWidth))
genHexCoord =
  Hex.HexCoord
    <$> vInAddrRange
    <*> vInAddrRange
    <*> vInAddrRange

simDuration :: Int
simDuration = 100

prop_read_ram :: H.Property
prop_read_ram = H.property $ do
  inp <-
    H.forAll (Gen.list (Range.singleton simDuration) genHexCoord)

  -- The output register of the BRAM is undefined on startup, therefore drop it
  let inputAddr = C.fromList inp
      -- Domain, clk, rst and ena must be defined due to VCD dump requiring it
      simOutSignal = C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen $ hexRam @TestDom @AddrWidth @DataD inputAddr (pure Nothing)
      simOut = drop 1 $ fromIntegral <$> C.sampleN (simDuration + 1) simOutSignal :: [Int]
      expected = replicate simDuration 3

  vcddata <- H.evalIO $ SW.dumpVCD (0, simDuration + 1) (SW.traceSignal "inputAddr" simOutSignal) []
  case vcddata of
    Left msg ->
      error msg
    Right (vcd, meta) -> do
      H.evalIO $ writeFile "waveforms/waveform.vcd" vcd
      H.evalIO $ SW.writeFileJSON "waveforms/waveform.json" meta

  -- Check that the simulated output matches the expected output
  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
