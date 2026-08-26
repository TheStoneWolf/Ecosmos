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

inp :: [Hex.HexCoord (C.Unsigned AddrWidth)]
inp = [Hex.HexCoord x y | x <- [0 .. 2 ^ nrAxisBits - 1], y <- [0 .. 2 ^ nrAxisBits - 1]]

genAddresses :: H.Gen (Hex.HexCoord (C.Unsigned AddrWidth))
genAddresses = Hex.HexCoord <$> x <*> y
  where
    x = genUnsigned $ Range.linear 0 axisMax
    y = genUnsigned $ Range.linear 0 axisMax
    axisMax = 2 ^ nrAxisBits - 1

-- ONLY READ
prop_only_read_ram :: H.Property
prop_only_read_ram = H.property $ do
  let inAddr = C.fromList $ C.cycle inp
      simOutSignal = C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen $ vegetation @TestDom @AddrWidth @DataWidth (Just <$> inAddr) (pure False)
      simOut = take nrAddresses . fmap fromIntegral . catMaybes $ C.sampleN simDuration (snd <$> simOutSignal) :: [Int]

      expected = fromIntegral <$> catMaybes (tickRamData 0)

  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

-- UPDATE THEN READ
testCircuit :: forall dom. (C.HiddenClockResetEnable dom) => C.Signal dom (Maybe (C.Unsigned DataWidth))
testCircuit =
  let updateStrobe = C.fromList $ True : True : repeat False
      startupCount = C.register 0 (startupCount + 1) :: C.Signal dom Integer
      startupDone = startupCount C..>=. 10
      isReading = C.register False (isReading C..||. (startupDone C..&&. isReady))

      input = C.mux isReading (C.fromList $ Just <$> C.cycle inp) (pure Nothing)

      (isReady, resultData) = C.unbundle $ vegetation @dom @AddrWidth @DataWidth input updateStrobe
   in resultData

prop_update_then_read :: H.Property
prop_update_then_read = H.property $ do
  let simOutSignal = C.unbundle $ C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen testCircuit
      simOutInt = C.sampleN simDuration simOutSignal
      simOut = take nrAddresses . fmap fromIntegral . catMaybes $ simOutInt :: [Int]

      expected = fromIntegral <$> catMaybes (tickRamData 1)

  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show simOut
  H.diff expected (==) simOut

vegetationTests :: TestTree
vegetationTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain vegetationTests
