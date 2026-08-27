{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Vegetation where

import Clash.Explicit.Prelude (Unsigned)
import Clash.Hedgehog.Sized.Unsigned
import qualified Clash.Prelude as C
import Data.List (elemIndex)
import Data.Maybe (catMaybes, mapMaybe)
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

calc :: Int -> Int -> Int
calc tick x = (3 + vegetationGainN * (tick + x `G.div` nrAddresses)) `G.mod` dataWidthN

tickRamData :: Int -> Maybe (Int, Int) -> [Int]
tickRamData tick Nothing = map (calc tick) [0 .. nrAddresses - 1]
tickRamData tick (Just (addr, overwriteData)) = map calcX [0 .. nrAddresses - 1]
  where
    calcX x
      | x == addr = overwriteData
      | otherwise = calc tick x

inp :: forall addrWidth. (C.KnownNat addrWidth) => [Hex.HexCoord (C.Unsigned addrWidth)]
inp = Hex.HexCoord <$> [C.minBound .. C.maxBound] <*> [C.minBound .. C.maxBound]

genAddresses :: forall addrWidth. (C.KnownNat addrWidth) => H.Gen (Hex.HexCoord (C.Unsigned addrWidth))
genAddresses = Hex.HexCoord <$> x <*> y
  where
    x = genUnsigned $ Range.linear 0 axisMax
    y = genUnsigned $ Range.linear 0 axisMax
    axisMax = 2 ^ nrAxisBits - 1

-- ONLY READ
-- Read out all elements to verify the default is correct and accessible
prop_only_read_ram :: H.Property
prop_only_read_ram = H.property $ do
  let inAddr = C.fromList $ G.cycle inp
      resultS =
        mapMaybe G.snd $
          C.sampleN simDuration $
            C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen $
              vegetation @TestDom @AddrWidth @DataWidth (Just <$> inAddr) (pure Nothing) (pure False)
      result = take nrAddresses $ fmap fromIntegral resultS :: [Int]

      expected = tickRamData 0 Nothing

  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show result
  H.diff expected (==) result

-- UPDATE THEN READ
-- Update all elements, i.e. run VegetationCalc on all of them and then read them out to verify they have updated
testCircuit2 :: forall dom. (C.HiddenClockResetEnable dom) => C.Signal dom (Maybe (C.Unsigned DataWidth))
testCircuit2 =
  let updateStrobe = C.fromList $ True : True : repeat False
      startupCount = C.register 0 (startupCount + 1) :: C.Signal dom Integer
      startupDone = startupCount C..>=. 10
      isReading = C.register False (isReading C..||. (startupDone C..&&. isReady))

      input = C.mux isReading (C.fromList $ Just <$> G.cycle inp) (pure Nothing)

      (isReady, resultData) = C.unbundle $ vegetation @dom @AddrWidth @DataWidth input (pure Nothing) updateStrobe
   in resultData

prop_update_then_read :: H.Property
prop_update_then_read = H.property $ do
  let resultS = C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen testCircuit2
      resultInt = C.sampleN simDuration resultS
      result = take nrAddresses . fmap fromIntegral . catMaybes $ resultInt :: [Int]

      expected = tickRamData 1 Nothing

  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show result
  H.diff expected (==) result

-- WRITE UPDATE THEN READ
-- Overwrite an element, then update and read out the values. The one overwritten element should have a different
-- value than the others that used the base default value
readSignal :: forall dom addrWidth. (C.HiddenClockResetEnable dom, C.KnownNat addrWidth) => C.Signal dom Bool -> C.Signal dom (Maybe (Hex.HexCoord (C.Unsigned addrWidth)))
-- Read out the list of addresses in order when enabled
readSignal = C.mealy next inp
  where
    next [] _ = ([], Nothing)
    next remaining False = (remaining, Nothing)
    next (addr : rest) True = (rest, Just addr)

testCircuit3 ::
  forall dom addrWidth dataWidth.
  (C.HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth, C.KnownNat dataWidth) =>
  (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth) -> C.Signal dom (Maybe (C.Unsigned dataWidth))
testCircuit3 wrPacket =
  let wrPacketSignal = C.fromList $ Nothing : Just wrPacket : repeat Nothing
      -- Wait for starting write to go through
      updateStrobe = C.fromList $ replicate 4 False ++ True : repeat False

      -- Since the DUT is ready on startup but that opportunity is for overwriting a value, skip that window by
      -- skipping the first 10 cycles. By that point, the Update process is surely on and the DUT no longer ready
      updateCount = C.register 0 (updateCount + 1) :: C.Signal dom Integer
      updateDone = updateCount C..>=. 10
      isReading = C.register False (isReading C..||. (updateDone C..&&. isReady))
      readS = readSignal isReading

      (isReady, resultData) = C.unbundle $ vegetation readS wrPacketSignal updateStrobe
   in resultData

prop_write_update_then_read :: H.Property
prop_write_update_then_read = H.property $ do
  wrAddr <- H.forAll genAddresses
  let wrPacket = (wrAddr, 1)

      resultSignal = C.withClockResetEnable @TestDom C.clockGen C.resetGen C.enableGen $ testCircuit3 @TestDom @AddrWidth @DataWidth wrPacket
      result = take nrAddresses . fmap C.numConvert . catMaybes $ C.sampleN simDuration resultSignal :: [Int]

      indexOverwritten = C.fromJustX $ elemIndex wrAddr inp
      expected = tickRamData 1 $ Just (indexOverwritten, 5)

  H.annotate $ "expected: " <> show expected
  H.annotate $ "received: " <> show result
  H.diff expected (==) result

vegetationTests :: TestTree
vegetationTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain vegetationTests
