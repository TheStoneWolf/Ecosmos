{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Vegetation where

import qualified Clash.Prelude as C
import Data.Proxy
import GHC.TypeNats (natVal)
import qualified Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Vegetation
import VegetationCalc (vegetationGain)
import Prelude hiding (writeFile)
import qualified Prelude as G

C.createDomain C.vSystem {C.vName = "TestDom", C.vPeriod = 1000}

type AddrWidth = 2

type DataWidth = 3

nrAddresses :: Int
-- Because AddrWidth is the per-axis width and there are two axes, the number
-- of levels before wrapping is the total bit width across both axes.
nrAddresses = 2 ^ (2 * (C.fromIntegral (C.natVal (Proxy @AddrWidth)) :: Int))

simDuration :: Int
simDuration = 3 * nrAddresses

dataWidthN :: Int
dataWidthN = 2 ^ (C.fromIntegral (natVal (Proxy :: Proxy DataWidth)) :: Int)

expected :: [Int]
expected = [(3 + vegetationGainN * (1 + x `G.div` nrAddresses)) `G.mod` dataWidthN | x <- [0 .. simDuration - 1]]

prop_read_init_ram :: H.Property
prop_read_init_ram = H.property $ do
  let simOutSignal = C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen $ vegetation @TestDom @AddrWidth @DataWidth Proxy
      -- 2 cycle delay to drop undefined value at start and 1 cycle delay from read -> output in bram
      simOut = drop 2 $ fromIntegral <$> C.sampleN (simDuration + 2) simOutSignal :: [Int]

  -- ##########
  -- NOTE Due to Clash bug (?) this will keep the test going forever or error if the test is successful. Do I have the patience to
  -- make a bug report? Probably not. This is useful when the test fails however as only then will it generate the VCD
  -- ##########
  -- vcddata <- H.evalIO $ SW.dumpVCD (0, simDuration + 1) (SW.traceSignal "simOut1" simOutSignal) ["oldAddr1", "simOut", "reAddr1", "curData1", "wrData1"]
  -- case vcddata of
  --   Left msg ->
  --     error msg
  --   Right (vcd, meta) -> do
  --     H.evalIO $ writeFile "waveforms/veg_waveform.vcd" vcd
  --     H.evalIO $ SW.writeFileJSON "waveforms/veg_waveform.json" meta

  -- Check that the simulated output matches the expected output
  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

vegetationGainN :: Int
vegetationGainN = fromIntegral (vegetationGain @DataWidth)

vegetationTests :: TestTree
vegetationTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain vegetationTests
