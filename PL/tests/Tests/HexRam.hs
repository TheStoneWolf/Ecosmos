{-# OPTIONS_GHC -Wno-orphans #-}

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

type AddrWidth = 8

type DataD = 8

hexRamLatency :: Int
hexRamLatency = 2

-- TODO Fix why the highest index element is not allowed in the bram
vInAddrRange :: H.Gen (C.Unsigned AddrWidth)
vInAddrRange = genUnsigned (Range.linear 0 ((maxBound :: C.Unsigned AddrWidth) - 1))

vInDataRange :: H.Gen (C.Unsigned DataD)
vInDataRange = genUnsigned (Range.linear 0 ((maxBound :: C.Unsigned DataD) - 1))

genHexCoord :: H.Gen (Hex.HexCoord (C.Unsigned AddrWidth))
genHexCoord =
  Hex.HexCoord
    <$> vInAddrRange
    <*> vInAddrRange
    <*> vInAddrRange

simDuration :: Int
simDuration = 100

prop_read_init_ram :: H.Property
prop_read_init_ram = H.property $ do
  inp <-
    H.forAll (Gen.list (Range.singleton simDuration) genHexCoord)

  let inputAddr = C.fromList inp
      -- Domain, clk, rst and ena must be defined due to VCD dump requiring it
      simOutSignal = C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen $ hexRam @TestDom @AddrWidth @DataD inputAddr (pure Nothing)
      -- The output register of the BRAM is undefined on startup, therefore drop it
      simOut = drop 1 $ fromIntegral <$> C.sampleN (simDuration + 1) simOutSignal :: [Int]
      expected = replicate simDuration 3

  vcddata <- H.evalIO $ SW.dumpVCD (0, simDuration + 1) (SW.traceSignal "inputAddr" simOutSignal) []
  case vcddata of
    Left msg ->
      error msg
    Right (vcd, meta) -> do
      H.evalIO $ writeFile "waveforms/re_waveform.vcd" vcd
      H.evalIO $ SW.writeFileJSON "waveforms/re_waveform.json" meta

  -- Check that the simulated output matches the expected output
  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

genHexData :: Hex.HexCoord (C.Unsigned AddrWidth) -> H.Gen (Maybe (Hex.HexCoord (C.Unsigned AddrWidth), C.Unsigned DataD))
genHexData addr = do
  dataD <- vInDataRange
  pure (Just (addr, dataD))

addrDelayedHexRam ::
  (C.HiddenClockResetEnable TestDom) =>
  [Hex.HexCoord (C.Unsigned AddrWidth)] ->
  [Maybe (Hex.HexCoord (C.Unsigned AddrWidth), C.Unsigned DataD)] ->
  C.Signal TestDom (C.Unsigned DataD)
addrDelayedHexRam inAddr inData = SW.traceSignal "outData" outData
  where
    addrSig = case inAddr of
      -- delay re addr so by one cycle so data can first be written to then be read
      -- the first
      firstAddr : _ -> C.register @TestDom firstAddr (SW.traceSignal "reAddr" $ C.fromList inAddr)
      [] -> error "inAddr must be non-empty"
    outData = hexRam @TestDom @AddrWidth @DataD (SW.traceSignal "addrDel" addrSig) (SW.traceSignal "wrData" $ C.fromList inData)

prop_write_read_ram :: H.Property
prop_write_read_ram = H.property $ do
  inpAddr <-
    H.forAll (Gen.list (Range.singleton simDuration) genHexCoord)
  inpDataRandom <-
    H.forAll $ traverse genHexData inpAddr

  let inpData = inpDataRandom
      simOutSignal =
        C.withClockResetEnable @TestDom
          C.clockGen
          C.resetGen
          C.enableGen
          $ addrDelayedHexRam inpAddr inpData
      simOut =
        -- Due to latency, drop the first elements as they have yet to reflect the test inputs
        drop hexRamLatency $
          fromIntegral <$> C.sampleN (simDuration + hexRamLatency) simOutSignal ::
          [Int]
      expected = dataInt <$> inpData
      dataInt Nothing = 3 -- Should not be reached
      dataInt (Just (_, dataD)) = fromIntegral dataD

  vcddata <- H.evalIO $ SW.dumpVCD (0, simDuration) simOutSignal ["reAddr", "wrData", "outData"]
  case vcddata of
    Left msg ->
      error msg
    Right (vcd, meta) -> do
      H.evalIO $ writeFile "waveforms/wrre_waveform.vcd" vcd
      H.evalIO $ SW.writeFileJSON "waveforms/wrre_waveform.json" meta

  -- Check that the simulated output matches the expected output
  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
