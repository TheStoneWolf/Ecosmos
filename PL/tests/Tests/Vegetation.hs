{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Vegetation where

import Clash.Hedgehog.Sized.Unsigned
import qualified Clash.Prelude as C
import Data.Maybe (catMaybes)
import Data.Proxy
import qualified Example.Hex as Hex
import GHC.TypeNats (natVal)
import qualified Hedgehog as H
import qualified Hedgehog.Range as Range
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

nrAxisBits :: Int
nrAxisBits = C.fromIntegral (C.natVal (Proxy @AddrWidth)) :: Int

-- | Because AddrWidth is the per-axis width and there are two axes, the number
-- of levels before wrapping is the total bit width across both axes.
type NrAddressesT = 2 C.^ (2 C.* AddrWidth)

nrAddresses :: Int
nrAddresses = C.natToNum @NrAddressesT

type Latency = 5

simDuration :: Int
simDuration = 3 * nrAddresses

dataWidthN :: Int
dataWidthN = 2 ^ (C.fromIntegral (natVal (Proxy @DataWidth)) :: Int)

vegetationGainN :: Int
vegetationGainN = fromIntegral (vegetationGain @DataWidth)

tickRamData :: Int -> [Maybe (C.Unsigned DataWidth)]
tickRamData tick = map (pure . fromIntegral . calc) [0 .. nrAddresses - 1]
  where
    calc x = (3 + vegetationGainN * (tick + x `G.div` nrAddresses)) `G.mod` dataWidthN

--
-- expData :: [Maybe (C.Unsigned DataWidth)]
-- expData = map (pure . fromIntegral . calc) [0 .. nrAddresses - 1]
--   where
--     calc x = (3 + vegetationGainN * (1 + x `G.div` nrAddresses)) `G.mod` dataWidthN
--
-- expected :: [Maybe (C.Unsigned DataWidth)]
-- expected = G.replicate (C.natToNum @Latency + nrAddresses) Nothing ++ expData

inp :: [Hex.HexCoord (C.Unsigned AddrWidth)]
inp = [Hex.HexCoord x y | x <- [0 .. 2 ^ nrAxisBits - 1], y <- [0 .. 2 ^ nrAxisBits - 1]]

expected :: [Int]
expected = fromIntegral <$> catMaybes (tickRamData 0)

genAddresses :: H.Gen (Hex.HexCoord (C.Unsigned AddrWidth))
genAddresses = Hex.HexCoord <$> x <*> y
  where
    x = genUnsigned $ Range.linear 0 axisMax
    y = genUnsigned $ Range.linear 0 axisMax
    axisMax = 2 ^ nrAxisBits - 1

prop_read_all_ram :: H.Property
prop_read_all_ram = H.property $ do
  let inAddr = C.fromList $ C.cycle inp
      simOutSignal = C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen $ vegetation @TestDom @AddrWidth @DataWidth (Just <$> inAddr) (pure False)
      simOut = take nrAddresses . fmap fromIntegral . catMaybes $ C.sampleN simDuration (snd <$> simOutSignal) :: [Int]

  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

simDuration2 :: Int
simDuration2 = length expected

-- prop_vegetation :: Property
-- prop_vegetation =
--   -- Run once, since hedgehog and possible other testing libraries seems to not like the first inputs being Nothing inside of Signal
--   -- By running deterministically with one test vector, the test can be performed
--   once $
--     property $
--       let delayCalcStart = C.fromList $ G.replicate nrAddresses True ++ G.repeat False
--           reAddr = C.fromList $ G.replicate (C.natToNum @Latency + nrAddresses) Nothing ++ (Just <$> inp) ++ G.repeat Nothing
--
--           clk = C.clockGen
--           rst = C.resetGen
--
--           simOutSignal = C.exposeClockResetEnable (vegetation @TestDom @AddrWidth @DataWidth reAddr delayCalcStart) clk rst C.enableGen
--           output = C.sampleN @TestDom simDuration2 simOutSignal
--           comparisonList = zip3 [0 :: Int ..] expected output
--           result = expected == output
--        in traceShow comparisonList result

vegetationTests :: TestTree
vegetationTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain vegetationTests
