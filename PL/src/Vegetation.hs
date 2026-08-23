module Vegetation
  ( vegetation,
  )
where

import Clash.Prelude
import Data.Maybe (fromMaybe, isJust)
import qualified Example.Hex as Hex
import HexRam (hexRam)
import VegetationCalc

data State (n :: Nat)
  = Idle
  | Update (Hex.HexCoord (Unsigned n))
  | UpdateFinish (Index 2)
  | Reading (Hex.HexCoord (Unsigned n))
  deriving (Generic, NFDataX)

fsm ::
  (KnownNat addrWidth) =>
  State addrWidth ->
  (Bool, Maybe (Hex.HexCoord (Unsigned addrWidth))) ->
  (State addrWidth, (Maybe (Hex.HexCoord (Unsigned addrWidth)), Bool, Bool))
-- Idle
fsm Idle (goNextTick, extRe)
  | goNextTick = (Update Hex.minHex, (Nothing, False, False))
  | isJust extRe = (Reading $ fromJustX extRe, (Nothing, True, False))
  | otherwise = (Idle, (Nothing, True, False))
-- Update
-- Iterate through all hexes and updates its plantmass
fsm (Update addr) (_, _) =
  if addr == Hex.maxHex
    then
      (UpdateFinish 2, (Just addr, False, True))
    else
      (Update (Hex.increment addr), (Just addr, False, True))
-- UpdateFinish
-- Due to not wanting an external Read to come simultaneously as the last is updated,
-- wait until the last updated hex is written to memory
fsm (UpdateFinish 0) (_, _) =
  (Idle, (Nothing, False, False))
fsm (UpdateFinish d) (_, _) =
  (UpdateFinish (d - 1), (Nothing, False, False))
-- Reading
-- Read the plantmass of the tile at the given address
fsm (Reading addr) (_, extRe)
  | isJust extRe = (Reading $ fromJustX extRe, (Just addr, True, False))
  | otherwise = (Idle, (Just addr, True, False))

-- TODO Put registers in front of inputs to ease placement

-- vegetation contains the plantmass amount for all tiles in the simulation. By raising goNextTick
-- high you go to the next simulation step which due to plant growth leads to the plantmass increasing
-- on all tiles. To get the plantmass for a tile send in a read address in extRe
vegetation ::
  forall dom addrWidth dataWidth.
  (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth, KnownNat dataWidth) =>
  Signal dom (Maybe (Hex.HexCoord (Unsigned addrWidth))) -> Signal dom Bool -> Signal dom (Bool, Maybe (Unsigned dataWidth))
vegetation extRe goNextTick = bundle (isReady, toExtData)
  where
    (reAddr, isReady, isUpdateRe) = mealyB fsm (Idle :: State addrWidth) (goNextTick, extRe)
    -- delay due to 1 clk cycle delay get result of read
    isUpdatePack = delay False isUpdateRe

    readPacket = memory reAddr wrData
    readData = fmap (fmap snd) readPacket
    toExtData = mux isUpdatePack (pure Nothing) readData
    toUpdateAddr = fromMaybe Hex.minHex <$> fmap (fmap fst) readPacket

    newData = vegetationTick <$> readData
    newPacket = mux isUpdatePack (liftA2 (\addr dat -> (addr,) <$> dat) toUpdateAddr newData) (pure Nothing)
    wrData = register Nothing newPacket

-- Container for hexRam encapsulating addr delay to keep connection to the data it was used to read. This is to make
-- updating easier as it must then be written back to that address
memory ::
  forall dom addrWidth dataWidth.
  (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth, KnownNat dataWidth) =>
  Signal dom (Maybe (Hex.HexCoord (Unsigned addrWidth))) ->
  Signal dom (Maybe (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth)) ->
  Signal dom (Maybe (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth))
memory reRequest wrData = readPacket
  where
    isValid = delay False $ isJust <$> reRequest
    -- NOTE Maybe passing along the read address is too messy and just delaying it in vegetation would be simpler
    reAddr = mux (isJust <$> reRequest) (fromJustX <$> reRequest) (pure Hex.minHex)

    -- Delay to match read request to output delay for hexRam
    packetAddr = delay Hex.minHex reAddr
    readPacket = mux isValid (Just <$> bundle (packetAddr, hexRam @dom @addrWidth reAddr wrData)) (pure Nothing)
