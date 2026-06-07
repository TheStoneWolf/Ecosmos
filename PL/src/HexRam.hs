module HexRam (hexRam) where

import Clash.Prelude
import qualified Example.Hex as Hex

hexRam ::
  forall dom addrWidth dataWidth.
  (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth, KnownNat dataWidth) =>
  Signal dom (Hex.HexCoord (Unsigned addrWidth)) -> Signal dom (Maybe (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth)) -> Signal dom (Unsigned dataWidth)
hexRam reCoordS wrCoordS = plantMass
  where
    plantMass = bRAM (linAddr <$> reCoordS) (wrAddr <$> wrCoordS)

wrAddr :: (Hex.AddrConstraints addrWidth, KnownNat dataWidth) => Maybe (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth) -> Maybe (Unsigned (2 * addrWidth), Unsigned dataWidth)
wrAddr Nothing = Nothing
wrAddr (Just (hexAddr, dataWidth)) = Just (linAddr hexAddr, dataWidth)

linAddr :: forall addrWidth. (Hex.AddrConstraints addrWidth) => Hex.HexCoord (Unsigned addrWidth) -> Unsigned (2 * addrWidth)
linAddr hexCoord = addr
  where
    x = Hex.x hexCoord
    y = Hex.y hexCoord

    -- TODO Think about if mapping the 3d-hexgrid coordinates to a 2d linear address space
    -- can be made more efficient somehow
    addr = shiftL (resize y) (natToNum @addrWidth) + resize x

bRAM ::
  forall dom addrWidth wrData.
  (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth, KnownNat wrData) =>
  Signal dom (Unsigned addrWidth) -> Signal dom (Maybe (Unsigned addrWidth, Unsigned wrData)) -> Signal dom (Unsigned wrData)
bRAM reAddr wrPacket = memOut
  where
    memOut = blockRam1 NoClearOnReset (SNat :: SNat (2 ^ addrWidth)) (3 :: Unsigned wrData) reAddr wrPacket
