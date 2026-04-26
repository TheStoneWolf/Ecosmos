module HexRam (hexRam) where

import Clash.Prelude
import Clash.Promoted.Nat
import qualified Example.Hex as Hex

maxDirBits = 4

maxDir = 4 * maxDirBits

hexRam ::
  (HiddenClockResetEnable dom, KnownNat a) =>
  Signal dom (Hex.HexCoord (Unsigned a)) -> Signal dom (Unsigned a)
hexRam coordS = plantMass
  where
    xS = Hex.x <$> coordS
    yS = Hex.y <$> coordS
    linAddr = (\y' x' -> y' * fromInteger maxDir + x') <$> yS <*> xS
    plantMass = bRAM linAddr

bRAM :: forall a dom. (HiddenClockResetEnable dom, KnownNat a) => Signal dom (Unsigned a) -> Signal dom (Unsigned a)
bRAM addr = memOut
  where
    memOut = blockRam (replicate (powSNat (SNat @2) (SNat @a)) 3) addr (pure Nothing)
