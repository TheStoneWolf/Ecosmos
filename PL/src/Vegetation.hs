module Vegetation
  ( vegetation,
  )
where

import Clash.Prelude
import Clash.Prelude.Testbench (assert)
import Data.Maybe (fromMaybe, isJust)
import qualified Example.Hex as Hex
import HexRam (hexRam)
import VegetationCalc

data State (addrWidth :: Nat) (dataWidth :: Nat)
  = Idle
  | Update (Hex.HexCoord (Unsigned addrWidth))
  | UpdateFinish (Index 3)
  | Write (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth)
  | Reading (Hex.HexCoord (Unsigned addrWidth))
  deriving (Generic, NFDataX)

-- TODO: Allow for write directly after read and vice versa
fsm ::
  (KnownNat addrWidth, KnownNat dataWidth) =>
  State addrWidth dataWidth ->
  (Bool, Maybe (Hex.HexCoord (Unsigned addrWidth)), Maybe (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth)) ->
  (State addrWidth dataWidth, (Maybe (Hex.HexCoord (Unsigned addrWidth)), Maybe (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth), Bool, Bool))
-- Idle
fsm Idle (goNextTick, extRe, extWr)
  | goNextTick = (Update Hex.minHex, (Nothing, Nothing, False, False))
  | isJust extWr = (Write $ fromJustX extWr, (Nothing, Nothing, False, False))
  | isJust extRe = (Reading $ fromJustX extRe, (Nothing, Nothing, True, False))
  | otherwise = (Idle, (Nothing, Nothing, True, False))
-- Update
-- Iterate through all hexes and updates its plantmass
fsm (Update addr) (_, _, _) =
  if addr == Hex.maxHex
    then
      (UpdateFinish 2, (Just addr, Nothing, False, True))
    else
      (Update (Hex.increment addr), (Just addr, Nothing, False, True))
-- UpdateFinish
-- Due to not wanting an external Read to come simultaneously as the last is updated,
-- wait until the last updated hex is written to memory
fsm (UpdateFinish 0) (_, _, _) =
  (Idle, (Nothing, Nothing, False, False))
fsm (UpdateFinish d) (_, _, _) =
  (UpdateFinish (d - 1), (Nothing, Nothing, False, False))
-- Write
fsm (Write wrPacket) (_, _, extWr)
  | isJust extWr = (Write $ fromJustX extWr, (Nothing, Just wrPacket, True, False))
  | otherwise = (Idle, (Nothing, Just wrPacket, True, False))
-- Reading
-- Read the plantmass of the tile at the given address
fsm (Reading addr) (_, extRe, _)
  | isJust extRe = (Reading $ fromJustX extRe, (Just addr, Nothing, True, False))
  | otherwise = (Idle, (Just addr, Nothing, True, False))

-- TODO Put registers in front of inputs to ease placement

-- vegetation contains the plantmass amount for all tiles in the simulation. By raising goNextTick
-- high you go to the next simulation step which due to plant growth leads to the plantmass increasing
-- on all tiles. To get the plantmass for a tile send in a read address in extRe. To overwrite any data
-- with external changes use extWr
vegetation ::
  forall dom addrWidth dataWidth.
  (HiddenClockResetEnable dom, Hex.AddrConstraints addrWidth, KnownNat dataWidth) =>
  Signal dom (Maybe (Hex.HexCoord (Unsigned addrWidth))) ->
  Signal dom (Maybe (Hex.HexCoord (Unsigned addrWidth), Unsigned dataWidth)) ->
  Signal dom Bool ->
  Signal dom (Bool, Maybe (Unsigned dataWidth))
vegetation extRe extWr goNextTick = bundle (isReady, toExtData)
  where
    (reAddr, extWrData, isReady, isUpdateRe) = mealyB fsm (Idle :: State addrWidth dataWidth) (goNextTick, extRe, extWr)
    -- delay due to 1 clk cycle delay get result of read
    isUpdatePack = delay False isUpdateRe

    readPacket =
      assert "You cannot write external data and write updated data to memory simultaneously" (areBothJust extWrData intWrData) (pure False)
        $ memory reAddr (mux (isJust <$> extWrData) extWrData intWrData)
    readData = fmap (fmap snd) readPacket
    toExtData = mux isUpdatePack (pure Nothing) readData
    toUpdateAddr = fromMaybe Hex.minHex <$> fmap (fmap fst) readPacket

    newData = vegetationTick <$> readData
    newPacket = mux isUpdatePack (liftA2 (\addr dat -> (addr,) <$> dat) toUpdateAddr newData) (pure Nothing)
    intWrData = register Nothing newPacket

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

areBothJust :: (Applicative f) => f (Maybe a1) -> f (Maybe a2) -> f Bool
{- HLINT ignore "Redundant <$>" -}
-- Doing the suggestion causes an error
areBothJust a b = isJust <$> a .&&. isJust <$> b
