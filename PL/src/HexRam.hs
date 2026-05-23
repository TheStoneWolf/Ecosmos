module HexRam (hexRam) where

import Clash.Prelude
import qualified Example.Hex as Hex

hexRam ::
  forall dom addrWidth.
  (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth) =>
  Signal dom (Hex.HexCoord (Unsigned addrWidth)) -> Signal dom (Unsigned addrWidth)
hexRam coordS = plantMass
  where
    xS = Hex.x <$> coordS
    yS = Hex.y <$> coordS

    -- TODO Think about if mapping the 3d-hexgrid coordinates to a 2d linear address space
    -- can be made more efficient somehow
    linAddr = (\y' x' -> y' * (natToNum @addrWidth) + x') <$> yS <*> xS
    plantMass = bRAM linAddr

bRAM :: forall dom addrWidth. (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth) => Signal dom (Unsigned addrWidth) -> Signal dom (Unsigned addrWidth)
bRAM addr = memOut
  where
    memOut = blockRam1 NoClearOnReset (SNat :: SNat (addrWidth ^ 2)) 3 addr (pure Nothing)
