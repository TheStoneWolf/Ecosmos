-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Example.Project where

import Clash.Prelude
import qualified Example.Hex as Hex
import HexRam

-- Create a domain with the frequency of your input clock. For this example we used
-- 50 MHz.
createDomain vSystem {vName = "Dom50", vPeriod = hzToPeriod 50e6}

-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}
topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Hex.HexCoord (Unsigned 8)) ->
  Signal Dom50 (Unsigned 8)
topEntity = exposeClockResetEnable hexRam

plusCoords ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom (Hex.HexCoord (Unsigned n)) ->
  Signal dom (Hex.HexCoord (Unsigned n))
plusCoords = fmap (Hex.HexCoord 2 (-2) 0 <>)
