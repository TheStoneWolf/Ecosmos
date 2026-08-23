module VegetationCalc (vegetationTick, vegetationGain) where

import Clash.Class.Num
import Clash.Prelude

vegetationGain :: (KnownNat dataWidth) => Unsigned dataWidth
vegetationGain = 4

-- | Calculate vegetation for next tick
--
-- >>> import Clash.Prelude
-- >>> vegetationTick @4 (Just 12)
-- Just 0
-- >>> import Clash.Prelude
-- >>> fmap (\value -> value - vegetationGain) (vegetationTick @4 (Just 5))
-- Just 5
vegetationTick :: (KnownNat dataWidth) => Maybe (Unsigned dataWidth) -> Maybe (Unsigned dataWidth)
vegetationTick (Just inData) = Just $ satAdd SatWrap inData vegetationGain
vegetationTick Nothing = Nothing
