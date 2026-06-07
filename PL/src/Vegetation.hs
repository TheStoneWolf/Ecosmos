module Vegetation where

import Clash.Prelude hiding (traceSignal)
import qualified Clash.Shockwaves as SW
import Data.Proxy
import qualified Example.Hex as Hex
import HexRam (hexRam)
import VegetationCalc

vegetation :: forall dom addrWidth dataWidth. (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth, KnownNat dataWidth) => Proxy addrWidth -> Signal dom (Unsigned dataWidth)
vegetation _ = newData
  where
    reAddr = register (Hex.HexCoord 0 0) (Hex.increment <$> reAddr)
    currentData = SW.traceSignal "curData1" $ hexRam @dom @addrWidth reAddr wrData

    oldAddr = register (Hex.HexCoord 0 0) reAddr
    newData = vegetationTick <$> currentData
    newPacket = Just <$> bundle (oldAddr, newData)
    wrData = register Nothing newPacket
